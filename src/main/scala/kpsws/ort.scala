package kpsws

import org.tresql.ORT
import org.tresql.NameMap
import org.tresql.Query
import xsdgen.ElementName
import scala.collection.JavaConversions._
import javax.xml.datatype._
import kpsws.impl._
import xsdgen.XsdTypeDef
import xsdgen.XsdGen
import xsdgen.XsdFieldDef
import xsdgen.Schema.xsdNameToDbName
import org.tresql.Env
import xsdgen.Schema
import xsdgen.JoinsParser

object ort extends org.tresql.NameMap {

  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance

  def pojoToMap(pojo: Any): Map[String, _] =
    if (pojo == null) Map.empty
    else pojo.getClass.getMethods filter (m =>
      m.getName.startsWith("get") && m.getName != "getClass"
        && m.getParameterTypes.size == 0) map (m =>
      m.getName.drop(3) -> (m.invoke(pojo) match {
        case null => null
        case x: String => x
        case c: Class[_] => c
        case x: XMLGregorianCalendar => x.toGregorianCalendar.getTime
        case x if (isPrimitive(x)) => x
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
          case x: String if t == classOf[Boolean] || t == classOf[java.lang.Boolean] => value match {
            case "Y" | "true" => m.invoke(pojo, java.lang.Boolean.TRUE)
            case "N" | "false" => m.invoke(pojo, java.lang.Boolean.FALSE)
            case null => m.invoke(pojo, java.lang.Boolean.FALSE)
            case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
          }
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

  def save(pojo: AnyRef): Long = {
    val viewDef = getViewDef(pojo.getClass)
    val tableName = viewDef.table
    val propMap = pojoToMap(pojo).map(e =>
      (xsdNameToDbName(e._1), xsdValueToDbValue(e._2)))
    val (id, isNew) = propMap.get("id").filter(_ != null).map(
      _.toString.toLong -> false) getOrElse (nextId(), true)
    if (isNew) ORT.insert(tableName, propMap + ("id" -> id))
    else ORT.update(tableName, propMap)
    id
  }

  def insertNoId(pojo: AnyRef) {
    val viewDef = getViewDef(pojo.getClass)
    val tableName = viewDef.table
    val propMap = pojoToMap(pojo).map(e =>
      (xsdNameToDbName(e._1), xsdValueToDbValue(e._2)))
    ORT.insert(tableName, propMap)
  }

  def save(pojo: AnyRef, id: Long) = {
    val viewDef = getViewDef(pojo.getClass)
    val tableName = viewDef.table
    ORT.save(tableName,
      pojoToMap(pojo).map(e => (xsdNameToDbName(e._1), e._2)) + ("id" -> id))
  }


  /* -------- Query support methods -------- */
  def getViewDef(viewClass: Class[_ <: AnyRef]) =
    XsdGen.xtd.get(ElementName.get(viewClass)) getOrElse
      XsdGen.xtd(ElementName.get(viewClass).replace("-", "_"))
  def query[T <: AnyRef](pojoClass: Class[T], params: ListRequestType): List[T] =
    query(getViewDef(pojoClass), pojoClass, params)
  def getOrNull[T <: AnyRef](viewClass: Class[T], id: Long): T = {
    val filterDef = Array(ListFilterType("Id", "=", id.toString))
    val sortDef = Array[ListSortType]()
    val req = ListRequestType(1, 0, filterDef, sortDef)
    ort.query(viewClass, req).headOption getOrElse null.asInstanceOf[T]
  }

  private def lowerNames(m: Map[String, _]) = m.map(e => (e._1.toLowerCase, e._2))
  // private def mapToPojo[T](m: Map[String, _], pojoClass: Class[T]): T = mapToPojo(lowerNames(m), pojoClass.newInstance)
  def selectToPojo[T](query: String, pojoClass: Class[T]) =  {
    def pojo(m: Map[String, _]) = mapToPojo(lowerNames(m), pojoClass.newInstance)
    Query.select(query).toListRowAsMap.map(pojo)
  }

  def query[T](view: XsdTypeDef, pojoClass: Class[T], params: ListRequestType) = {
    val tresqlQuery = queryString(view, params)
    Env.log(tresqlQuery._1)
    
    def pojo(m: Map[String, _]) = mapToPojo(lowerNames(m), pojoClass.newInstance)
    Query.select(tresqlQuery._1, tresqlQuery._2: _*).toListRowAsMap.map(pojo)
  }

  def queryString(view: XsdTypeDef, params: ListRequestType) = {
    import params.{ Filter => filter, Sort => sort, Limit => limit, Offset => offset }

    //base table alias
    val B = JoinsParser(view.joins).filter(_._2 == view.table).toList match {
      case (a, _) :: Nil => // if only one base table encountered return alias
        Option(a) getOrElse view.table
      case _ => "b" // default base table alias 
    }

    def queryColName(f: XsdFieldDef) =
      (if (f.tableAlias != null) f.tableAlias
      else if (f.table == view.table) B else f.table) + "." + f.name

    val cols = view.fields.map(f =>
      queryColName(f) + Option(f.alias).map(" " + _).getOrElse("")).mkString(" {", ", ", "}")

    val from = if (view.joins != null) view.joins else {
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table)
      if (tables.size > 1) {
        tables -= view.table
        // B is base table alias, ? is outer join
        tables.map(B + "/" + _ + "?").mkString(view.table + " ", "; ", "")
      } else view.table + " " + B
    }
    import Schema.{ dbNameToXsdName => xsdName }
    val fieldNameToDefMap = view.fields.map(f => xsdName(Option(f.alias) getOrElse f.name) -> f).toMap
    def fieldNameToDef(f: String) = fieldNameToDefMap.getOrElse(f,
      sys.error("Field " + f + " is not available from view " + xsdName(view.name)))

    val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
    def comparison(comp: String) =
      if (ComparisonOps.contains(comp)) comp
      else sys.error("Comparison operator not supported: " + comp)

    val where = if (filter == null || filter.size == 0) "" else filter.map(f =>
      queryColName(fieldNameToDef(f.Field)) + " " + comparison(f.Comparison) +
        " ?").mkString("[", " & ", "]")

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

    val values = if (filter == null) Array[String]() else filter.map(f => {
      val v = f.Value
      // TODO describe convertion error (field, table, value, ...)
      Schema.getCol(view, fieldNameToDef(f.Field)).xsdType.name match {
        case "string" => v
        case "int" => v.toInt
        case "long" => v.toLong
        case "integer" => BigInt(v)
        case "decimal" => BigDecimal(v)
        case "date" => Format.xsdDate.parse(v)
        case "dateTime" => Format.xsdDateTime.parse(v)
        case "boolean" => v match {
          case "true" => "Y"
          case "false" => "N"
          case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
        }
        case x => sys.error("Filter value type not supported: " + x)
      }
    })

    val (q, limitOffsetPars) = limitOffset(from + where + cols + order)
    (q, values ++ limitOffsetPars)
  }

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)
}
