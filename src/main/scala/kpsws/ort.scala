package kpsws

import org.tresql.ORT
import org.tresql.NameMap
import org.tresql.Query
import xsdgen.ElementName
import scala.collection.JavaConversions._
import javax.xml.datatype._
import kpsws.impl._
import metadata.XsdTypeDef
import metadata.XsdFieldDef
import metadata.DbConventions.xsdNameToDbName
import org.tresql.Env
import java.util.Date
import java.security.MessageDigest
import java.text.SimpleDateFormat
import metadata.JoinsParser
import metadata.Metadata
import metadata.YamlViewDefLoader

object ort extends org.tresql.NameMap {

  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance

  private def propName(m: java.lang.reflect.Method) = {
    val mName = m.getName
    if (mName.startsWith("get") && mName.length > 3 &&
      mName.charAt(3).isUpper) mName.substring(3)
    else if (mName.startsWith("is") && mName.length > 2 &&
      mName.charAt(2).isUpper) mName.substring(2)
    else throw new RuntimeException(
      "Failed to extract property name from method name: " + mName)
  }
  def pojoToMap(pojo: Any): Map[String, _] =
    if (pojo == null) Map.empty
    else pojo.getClass.getMethods filter (m =>
      m.getName.startsWith("get") && m.getName != "getClass"
        || m.getName.startsWith("is")) filter (m =>
      m.getParameterTypes.size == 0) map (m =>
      propName(m) -> (m.invoke(pojo) match {
        case null => null
        case x: String => x
        case c: Class[_] => c
        case x: XMLGregorianCalendar => x.toGregorianCalendar.getTime
        case x if (isPrimitive(x)) => x
        // FIXME blob support - case b: Array[Byte] => b
        case l: Seq[_] => l map pojoToMap
        case l: Array[_] => l map pojoToMap
        case l: java.util.Collection[_] => l map pojoToMap
        case x => throw new RuntimeException(
          "Pojo map not implemented - class: " + x.getClass + ", value: " + x)
      })) toMap

  def mapToPojo[T](map: Map[String, _], pojo: T): T = {
    pojo.getClass.getMethods.filter(m => m.getName.startsWith("set") &&
      m.getParameterTypes.size == 1) foreach { m =>
      val propName = m.getName.drop(3) //property name
      val propClass = propToClassName(propName) //find class name in the case value is map or list i.e. not primitive object
      val t = m.getParameterTypes()(0)
      map.get(xsdNameToDbName(propName)).map(value => try {
        value match {
          //this may require tuning since it is difficult with java reflection to determine compatibility
          //between map entry value type and setter parameter type
          //hopefully scala reflection will help with this
          case d: BigDecimal => {
            if (t == classOf[Int] || t == classOf[java.lang.Integer])
              m.invoke(pojo, new java.lang.Integer(d.toInt))
            else if (t == classOf[Long] || t == classOf[java.lang.Long])
              m.invoke(pojo, new java.lang.Long(d.toLong))
            else if (t == classOf[Double] || t == classOf[java.lang.Double])
              m.invoke(pojo, d.doubleValue.asInstanceOf[Object])
            else if (t == classOf[java.math.BigDecimal])
              m.invoke(pojo, d.bigDecimal)
            else if (t == classOf[java.math.BigInteger])
              m.invoke(pojo, d.bigDecimal.unscaledValue)
            else try m.invoke(pojo, d)
          }
          case x if (t == classOf[java.math.BigInteger] && x != null) =>
            m.invoke(pojo, new java.math.BigInteger(x.toString))
          case x: java.util.Date if (t == classOf[XMLGregorianCalendar]) => {
            val gc = new java.util.GregorianCalendar()
            gc.setTime(x)
            m.invoke(pojo, XML_DATATYPE_FACTORY.newXMLGregorianCalendar(gc))
          }
          case inMap: Map[String, _] if (t == Class.forName(propClass)) => m.invoke(pojo, mapToPojo(inMap,
            Class.forName(propClass).newInstance).asInstanceOf[Object])
          //may be exact collection which is used in xsd generated pojos must be used?
          case Seq() if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => t.newInstance
          case s: Seq[Map[String, _]] if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => {
            val col: java.util.Collection[_] = s.map(mapToPojo(_, Class.forName(propClass).newInstance))
            m.invoke(pojo, col)
          }
          case x: String if t == classOf[Boolean] || t == classOf[java.lang.Boolean] => toLowerOrNull(value.toString) match {
            case "y" | "true" => m.invoke(pojo, java.lang.Boolean.TRUE)
            case "n" | "false" => m.invoke(pojo, java.lang.Boolean.FALSE)
            case null => m.invoke(pojo, java.lang.Boolean.FALSE)
            case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
          }
          case blob: java.sql.Blob if t == classOf[Array[Byte]] =>
            m.invoke(pojo, blob.getBytes(1, blob.length.toInt)) // FIXME toInt!
          case x => m.invoke(pojo, x.asInstanceOf[Object])
        }
      } catch {
        case ex: Exception =>
          throw new RuntimeException("Failed to invoke setter " + m.getName +
            "(" + t.getName + ") with value " + value +
            " of class " + value.getClass.getName, ex)
      })
    }
    pojo
  }

  private def toLowerOrNull(value: String) = if (value == null) value else value.toLowerCase

  private def isPrimitive[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null || (x match {
    case _: java.lang.Number | _: java.lang.Boolean | _: java.util.Date | _: XMLGregorianCalendar => true
    case _ => false
  })
  private def propToClassName(prop: String) = if (prop.endsWith("List")) prop.dropRight(4) else prop

  private def xsdValueToDbValue(xsdValue: Any) = xsdValue match {
    case true => "Y"
    case false => "N"
    case x => x
  }

  private def nextId() = Query.unique[Long]("dual{seq.nextval}")

  private def pojoToSaveableMap(pojo: AnyRef, viewDef: XsdTypeDef) = {
    import metadata.{ Metadata => Schema }
    val propMap = pojoToMap(pojo).map(e =>
      (xsdNameToDbName(e._1), xsdValueToDbValue(e._2)))
    val modificationDateField =
      Schema.tableDef(viewDef).cols.find(_.name == "last_modified")
    val checksumField =
      Schema.tableDef(viewDef).cols.find(_.name == "record_checksum")
    val idField =
      Schema.tableDef(viewDef).cols.find(_.name == "id")
    val createdByField =
      if (idField.isDefined &&
        propMap.get("id").filter(v => v != null && v != "") == None)
        Schema.tableDef(viewDef).cols.find(_.name == "created_by_id")
      else None
    val modifiedByField =
      Schema.tableDef(viewDef).cols.find(_.name == "last_modified_by_id")
    val lastModifiedDate =
      if (modificationDateField.isDefined || checksumField.isDefined) new Date()
      else null
    def checksum = MessageDigest.getInstance("MD5").digest(
      new SimpleDateFormat("yyyy.MM.dd hh24:mm:ss.SSS")
        .format(lastModifiedDate).getBytes).map("%02X".format(_)).mkString
    import kpsws.RequestContext.userId
    def trim(value: Any) = value match {
      case s: String => s.trim()
      case x => x
    }

    propMap.map(e => (e._1, trim(e._2))) ++ List(
      modificationDateField.map(f => ("last_modified" -> lastModifiedDate)),
      checksumField.map(f => ("record_checksum" -> checksum)),
      createdByField.map(f => ("created_by_id" -> userId)),
      modifiedByField.map(f => ("last_modified_by_id" -> userId)))
      .flatMap(x => x)
  }

  def save(pojo: AnyRef): Long = {
    val viewDef = Metadata.getViewDef(pojo.getClass)
    val tableName = viewDef.table
    val propMap = pojoToSaveableMap(pojo, viewDef)
    val (id, isNew) = propMap.get("id").filter(_ != null).map(
      _.toString.toLong -> false) getOrElse (nextId(), true)
    if (isNew) ORT.insert(tableName, propMap + ("id" -> id))
    else ORT.update(tableName, propMap)
    id
  }

  /*
  def insertNoId(pojo: AnyRef) {
    val viewDef = Metadata.getViewDef(pojo.getClass)
    val tableName = viewDef.table
    val propMap = pojoToMap(pojo).map(e =>
      (xsdNameToDbName(e._1), xsdValueToDbValue(e._2)))
    ORT.insert(tableName, propMap)
  }
*/

  def save(pojo: AnyRef, id: Long) = {
    val viewDef = Metadata.getViewDef(pojo.getClass)
    val tableName = viewDef.table
    val propMap = pojoToSaveableMap(pojo, viewDef)
    ORT.save(tableName, propMap + ("id" -> id))
  }

  /* -------- Query support methods -------- */
  def countAll[T <: AnyRef](pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map())) = {
    val filter = params.copy(Limit = 0, Offset = 0, Sort = Array())
    // FIXME Needs some optimization :)
    query(Metadata.getViewDef(pojoClass), pojoClass, filter, wherePlus).size
  }
  def query[T <: AnyRef](pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map())): List[T] =
    query(Metadata.getViewDef(pojoClass), pojoClass, params, wherePlus)
  def getOrNull[T <: AnyRef](viewClass: Class[T], id: Long): T = {
    val filterDef = Array(ListFilterType("Id", "=", id.toString))
    val sortDef = Array[ListSortType]()
    val req = ListRequestType(1, 0, filterDef, sortDef)
    ort.query(viewClass, req).headOption getOrElse null.asInstanceOf[T]
  }

  private def lowerNames(m: Map[String, _]) = m.map(e => (e._1.toLowerCase, e._2))
  // private def mapToPojo[T](m: Map[String, _], pojoClass: Class[T]): T = mapToPojo(lowerNames(m), pojoClass.newInstance)
  def selectToPojo[T](query: String, pojoClass: Class[T]) = {
    def pojo(m: Map[String, _]) = mapToPojo(lowerNames(m), pojoClass.newInstance)
    Query.select(query).toListRowAsMap.map(pojo)
  }

  def query[T](view: XsdTypeDef, pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any])) = {
    val tresqlQuery = queryString(view, params, wherePlus)
    Env.log(tresqlQuery._1)

    def pojo(m: Map[String, _]) = mapToPojo(lowerNames(m), pojoClass.newInstance)
    Query.select(tresqlQuery._1, tresqlQuery._2).toListRowAsMap.map(pojo)
  }

  def queryString(view: XsdTypeDef, params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map())) = {
    val paramsFilter =
      Option(params).map(_.Filter).filter(_ != null).map(_.toList) getOrElse Nil
    val filteredParams = params.copy(Filter =
      paramsFilter.filter(f => !wherePlus._2.contains(f.Field)).toArray)
    import filteredParams.{ Filter => filter, Sort => sort, Limit => limit, Offset => offset }

    //base table alias
    val B = JoinsParser(view.joins).filter(_._2 == view.table).toList match {
      case (a, _) :: Nil => // if only one base table encountered return alias
        Option(a) getOrElse view.table
      case _ => "b" // default base table alias 
    }

    def queryColName(f: XsdFieldDef) =
      (if (f.tableAlias != null) f.tableAlias
      else if (f.table == view.table) B else f.table) + "." + f.name

    val cols = view.fields.filter(!_.isExpression).map(f =>
      queryColName(f) + Option(f.alias).map(" " + _).getOrElse("")).mkString(" {", ", ", "}")

    val from = if (view.joins != null) view.joins else {
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table)
      if (tables.size > 1) {
        tables -= view.table
        // B is base table alias, ? is outer join
        tables.map(B + "/" + _ + "?").mkString(view.table + " ", "; ", "")
      } else view.table + " " + B
    }
    import metadata.DbConventions.{ dbNameToXsdName => xsdName }
    val fieldNameToDefMap = view.fields.map(f => xsdName(Option(f.alias) getOrElse f.name) -> f).toMap
    def fieldNameToDef(f: String) = fieldNameToDefMap.getOrElse(f,
      sys.error("Field " + f + " is not available from view " + xsdName(view.name)))

    val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
    def comparison(comp: String) =
      if (ComparisonOps.contains(comp)) comp
      else sys.error("Comparison operator not supported: " + comp)

    val where = (filter.map(f =>
      queryColName(fieldNameToDef(f.Field)) + " " + comparison(f.Comparison) +
        " :" + f.Field) ++ Option(wherePlus._1).filter(_ != ""))
      .mkString("[", " & ", "]")

    val order =
      if (sort == null || sort.size == 0) ""
      else sort.map(s => (if (s.Order == "desc") "~" else "") +
        "#(" + queryColName(fieldNameToDef(s.Field)) + ")").mkString("")

    def limitOffset(query: String) = (limit, offset) match {
      case (0, 0) => (query, Array())
      case (limit, 0) => // TODO no need for subquery here
        ("/(" + query + ") [rownum <= ?]", Array(limit))
      case (0, offset) =>
        ("/(/(" + query + ") w {rownum rnum, w.*}) [rnum > ?]", Array(offset))
      case (limit, offset) =>
        ("/(/(" + query + ") w [rownum <= ?] {rownum rnum, w.*}) [rnum > ?]",
          Array(offset + limit, offset))
    }

    val values = if (filter == null) Map[String, Any]() else filter.map(f => {
      val v = f.Value
      // TODO describe convertion error (field, table, value, ...)
      f.Field -> (Metadata.getCol(view, fieldNameToDef(f.Field)).xsdType.name match {
        case "string" => v
        case "int" => v.toInt
        case "long" => v.toLong
        case "integer" => BigInt(v)
        case "decimal" => BigDecimal(v)
        case "date" => Format.xsdDate.parse(v)
        case "dateTime" => Format.xsdDateTime.parse(v)
        case "boolean" => toLowerOrNull(v) match {
          case "true" => "Y"
          case "false" => "N"
          case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
        }
        case x => sys.error("Filter value type not supported: " + x)
      })
    }).toMap

    val (q, limitOffsetPars) = limitOffset(from + where + cols + order)
    (q, values ++ wherePlus._2 ++ limitOffsetPars.zipWithIndex.map(t=> (t._2 + 1).toString -> t._1).toMap)
  }

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)
}
