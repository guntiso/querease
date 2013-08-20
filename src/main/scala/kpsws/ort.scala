package kpsws

import org.tresql.ORT
import org.tresql.NameMap
import org.tresql.Query
import xsdgen.ElementName
import language.postfixOps
import scala.collection.JavaConversions._
import javax.xml.datatype._
import kpsws.impl._
import kpsws.impl.Error._
import metadata.XsdTypeDef
import metadata.XsdFieldDef
import metadata.DbConventions.xsdNameToDbName
import org.tresql.Env
import java.util.Date
import java.security.MessageDigest
import java.text.SimpleDateFormat
import metadata.JoinsParser
import metadata.Join
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
          case inMap: Map[_, _] if (t == Class.forName(propClass)) =>
            m.invoke(pojo, mapToPojo(inMap.asInstanceOf[Map[String, _]],
              Class.forName(propClass).newInstance).asInstanceOf[Object])
          //may be exact collection which is used in xsd generated pojos must be used?
          case Seq() if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => t.newInstance
          case s: Seq[_] if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => {
            val col: java.util.Collection[_] = s.asInstanceOf[Seq[Map[String, _]]]
              .map(mapToPojo(_, Class.forName(propClass).newInstance))
            m.invoke(pojo, col)
          }
          case x: String if t == classOf[Boolean] || t == classOf[java.lang.Boolean] => x match {
            case "y" | "Y" | "true" | "TRUE" => m.invoke(pojo, java.lang.Boolean.TRUE)
            case "n" | "N" | "false" | "FALSE" => m.invoke(pojo, java.lang.Boolean.FALSE)
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

  private def isPrimitive[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null || (x match {
    case _: java.lang.Number | _: java.lang.Boolean | _: java.util.Date | _: XMLGregorianCalendar => true
    case _ => false
  })
  private def propToClassName(prop: String) = if (prop.endsWith("List")) prop.dropRight(4) else prop

  private def xsdValueToDbValue(xsdValue: Any) = xsdValue match {
    case true => "Y"
    case false => "N"
    // avoid unfriendly oracledb error message
    case x: String if x.length > 1000 && x.getBytes("UTF-8").length > 4000 =>
      err(TEXT_TOO_LONG, "" + x.getBytes("UTF-8").length)
    case x => x
  }

  private def nextId() = Query.unique[Long]("dual{seq.nextval}")

  def getChecksum(lastModifiedDate: Date) = MessageDigest.getInstance("MD5").digest(
    new SimpleDateFormat("yyyy.MM.dd hh24:mm:ss.SSS")
      .format(lastModifiedDate).getBytes).map("%02X".format(_)).mkString

  def getCurrentUserId = {
    val currentUserId = RequestContext.userId
    if (currentUserId <= 0)
      throw new RuntimeException("Request context - missing userId")
    else currentUserId
  }

  def pojoToSaveableMap(pojo: AnyRef, viewDef: XsdTypeDef) = {
    import metadata.{ Metadata => Schema }
    def toDbFormat(m: Map[String, _]): Map[String, _] = m.map {
      case (k, vList: List[Map[String, _]]) =>
        (xsdNameToDbName(k), vList map toDbFormat)
      case (k, v) => (xsdNameToDbName(k), xsdValueToDbValue(v))
    }
    val propMap = toDbFormat(pojoToMap(pojo))
    val modificationDateField =
      Schema.tableDef(viewDef).cols.find(_.name == "last_modified")
    val checksumField =
      Schema.tableDef(viewDef).cols.find(_.name == "record_checksum")
    val idField =
      Schema.tableDef(viewDef).cols.find(_.name == "id")
    val idOption =
      if (idField.isDefined)
        propMap.get("id")
          .filter(v => v != null && v != "")
          .map(_.toString.toLong)
      else None
    val createdByField =
      if (idField.isDefined && idOption == None)
        Schema.tableDef(viewDef).cols.find(_.name == "created_by_id")
      else None
    val modifiedByField =
      Schema.tableDef(viewDef).cols.find(_.name == "last_modified_by_id")
    val lastModifiedDate =
      if (modificationDateField.isDefined || checksumField.isDefined) new Date()
      else null
    if (idOption.isDefined && checksumField.isDefined) {
      val oldChecksum = Query.unique[String](
        viewDef.table + "[id=?]{record_checksum}", idOption.get)
      if (oldChecksum != propMap.getOrElse("record_checksum", ""))
        err(SYS_ERR_SOFT_LOCK_OCCURED)
    }
    def checksum = getChecksum(lastModifiedDate)

    lazy val userId = getCurrentUserId
    def trim(value: Any) = value match {
      case s: String => s.trim()
      case x => x
    }
    def isSaveable(fieldName: String) =
      viewDef.fields.find(f => f.name == fieldName && f.isExpression).isEmpty

    def toSingular(s: String) = // XXX to undo JAXB plural
      if (s endsWith "ies") s.dropRight(3) + "y"
      else if (s endsWith "s") s.dropRight(1)
      else s

    def toSaveableDetails(propMap: Map[String, Any], viewDef: XsdTypeDef): Map[String, Any] =
      propMap.map {
        case entry @ (key, l: List[Map[String, _]]) =>
          val fieldName = toSingular(key) // XXX undo JAXB plural 
          val fieldDef = viewDef.fields.find(_.name == fieldName)
            .getOrElse(sys.error("Field not found for property: " + viewDef.name + "." + fieldName))
          if (isSaveable(fieldDef.name)) {
            val childViewDef =
              Metadata.nameToViewDef.getOrElse(fieldDef.xsdType.name,
                sys.error("Child viewDef not found for field " + fieldName))
            childViewDef.table -> l.map(toSaveableDetails(_, childViewDef))
          } else entry
        case x => x
      }
    toSaveableDetails(propMap, viewDef)
      .map(e => (e._1, trim(e._2)))
      .filter(e => isSaveable(e._1)) ++
      List(
        modificationDateField.map(f => ("last_modified" -> lastModifiedDate)),
        checksumField.map(f => ("record_checksum" -> checksum)),
        createdByField.map(f => ("created_by_id" -> userId)),
        modifiedByField.map(f => ("last_modified_by_id" -> userId)))
      .flatMap(x => x)
  }

  // addParams allows to specify additional columns to be saved that are not present in pojo.
  def save(pojo: AnyRef, addParams: Map[String, Any] = null, forceInsert: Boolean = false): Long =
    saveTo(getViewDef(pojo).table, pojo, addParams, forceInsert)

  def saveTo(tableName: String, pojo: AnyRef, addParams: Map[String, Any] = null,
    forceInsert: Boolean = false): Long = {
    val pojoPropMap = pojoToSaveableMap(pojo, getViewDef(pojo))
    val propMap = if (addParams != null) pojoPropMap ++ addParams else pojoPropMap
    val (id, isNew) = propMap.get("id").filter(_ != null).map(
      _.toString.toLong -> forceInsert) getOrElse (nextId(), true)
    if (isNew) ORT.insert(tableName, propMap + ("id" -> id))
    else ORT.update(tableName, propMap)
    id
  }
  def getViewDef(pojo: AnyRef) = Metadata.getViewDef(pojo.getClass)

  def save(pojo: AnyRef, id: Long) = {
    val viewDef = getViewDef(pojo)
    val tableName = viewDef.table
    val propMap = pojoToSaveableMap(pojo, viewDef)
    ORT.save(tableName, propMap + ("id" -> id))
  }

  /* -------- Query support methods -------- */
  def countAll[T <: AnyRef](pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map())) = {
    val tresqlQuery =
      queryString(Metadata.getViewDef(pojoClass), params, wherePlus, true)
    Env.log(tresqlQuery._1)
    Query.unique[Int](tresqlQuery._1, tresqlQuery._2)
  }
  def query[T <: AnyRef](pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map())): List[T] =
    query(Metadata.getViewDef(pojoClass), pojoClass, params, wherePlus)
  def getOrNull[T <: AnyRef](viewClass: Class[T], id: Long,
    wherePlus: (String, Map[String, Any])): T = {
    val filterDef = Array(new ListFilterType("Id", "=", id.toString))
    val sortDef = Array[ListSortType]()
    val req = ListRequestType(1, 0, filterDef, sortDef)
    ort.query(viewClass, req, wherePlus).headOption getOrElse null.asInstanceOf[T]
  }

  private def lowerNames(m: Map[String, _]) = m.map(e => (e._1.toLowerCase, e._2))
  def selectToPojo[T](query: String, pojoClass: Class[T], params: Map[String, Any] = null) = {
    def pojo(m: Map[String, _]) = mapToPojo(lowerNames(m), pojoClass.newInstance)
    Query.select(query, params).toListRowAsMap.map(pojo)
  }

  def query[T](view: XsdTypeDef, pojoClass: Class[T], params: ListRequestType,
    wherePlus: (String, Map[String, Any])) = {
    val tresqlQuery = queryString(view, params, wherePlus)
    Env.log(tresqlQuery._1)
    selectToPojo(tresqlQuery._1, pojoClass, tresqlQuery._2)
  }

  val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  def comparison(comp: String) =
    if (ComparisonOps.contains(comp)) comp
    else sys.error("Comparison operator not supported: " + comp)

  def queryString(view: XsdTypeDef, params: ListRequestType,
    wherePlus: (String, Map[String, Any]) = (null, Map()),
    countAll: Boolean = false) = {
    val paramsFilter =
      Option(params).map(_.Filter).filter(_ != null).map(_.toList) getOrElse Nil
    import metadata.DbConventions.{ dbNameToXsdName => xsdName }
    val fieldNameToDefMap = view.fields.map(f => xsdName(Option(f.alias) getOrElse f.name) -> f).toMap
    // FIXME extra order by, injection-safe!
    val safeExpr = List("decode(cnt, null, 0, 1)",
      "decode(sign(next_reregistration_date - sysdate), 1, 0, 0, 0, 1)")
      .map(expr => (expr,
        XsdFieldDef("", "", "", "", false, true, expr, true, null, false, "")))
      .toMap
    def fieldNameToDef(f: String) = fieldNameToDefMap.getOrElse(f,
      safeExpr.get(f) getOrElse
        sys.error("Field " + f + " is not available from view " + xsdName(view.name)))
    def isFilterable(f: ListFilterType): Boolean =
      if (fieldNameToDef(f.Field).isExpression) sys.error("Calculated field " + f.Field +
        " is not available for filtering from view " + xsdName(view.name))
      else true
    val filteredParams = params.copy(Filter =
      paramsFilter.filter(f => !wherePlus._2.contains(f.Field)).filter(isFilterable).toArray)
    import filteredParams.{ Sort => sort, Offset => offset }
    //LIMIT threshold
    val limit = math.min(100, filteredParams.Limit)
    //list of tuples (bind variable name -> ListFilterType)
    val filter = filteredParams.Filter.groupBy(_.Field).toList.flatMap(f =>
      if (f._2.size > 1) f._2.zipWithIndex.map(t => (f._1 + t._2) -> t._1) else
        f._2.toList.map(f._1 -> _)).toArray

    //base table alias
    val B = JoinsParser(view.table, view.joins).filter(_.table == view.table).toList match {
      case Join(a, _, _) :: Nil => // if only one base table encountered return alias
        Option(a) getOrElse view.table
      case _ => "b" // default base table alias 
    }
    val langs = RequestContext.languagePreferences
    val preferRu = langs(0) == "ru"
    val lSuff = langs.map {
      case "lv" => ""
      case "en" => "_eng"
      case "ru" => "_rus"
    }
    def isRu(f: XsdFieldDef) = preferRu && f.isI18n
    def isI18n(f: XsdFieldDef) = f.isI18n
    def queryColTableAlias(f: XsdFieldDef) =
      Option(f.tableAlias) getOrElse
        (if (f.table == view.table) B else f.table)

    def queryColExpression(f: XsdFieldDef) = {
      val qName = queryColTableAlias(f) + "." + f.name
      if (isI18n(f)) lSuff.tail.foldLeft(qName + lSuff(0))((expr, suff) =>
        "nvl(" + expr + ", " + qName + suff + ")")
      else qName
    }

    def queryColAlias(f: XsdFieldDef) =
      Option(f.alias).getOrElse(if (isI18n(f)) f.name else null)

    def queryColName(f: XsdFieldDef) =
      Option(f.alias).getOrElse(
        if (isI18n(f)) f.name else queryColTableAlias(f) + "." + f.name)

    val cols =
      if (countAll) " {count(*)}"
      else view.fields.filter(!_.isExpression).filter(!_.isCollection).map(f =>
        queryColExpression(f)
          + Option(queryColAlias(f)).map(" " + _).getOrElse(""))
        .mkString(" {", ", ", "}")

    //DELEME when next todo done
    val from = if (view.joins != null) view.joins else {
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table)
      if (tables.size > 1) {
        tables -= view.table
        // B is base table alias, ? is outer join
        tables.map(B + "/" + _ + "?").mkString(view.table + " ", "; ", "")
      } else view.table + " " + B
    }
    /* TODO merge joins, outer join intelligently (according to metadata)
    val from = {
      val jtables = Option(view.joins).map(JoinsParser(view.table, _).map(_.table).toSet) getOrElse Set()
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table) -- jtables
      val autoBase = if (!jtables.contains(view.table)) view.table + " " + B else null 
      val autoJoins =
        if (tables.size > 1) {
          tables -= view.table
          // B is base table alias, ? is outer join
          tables.map(B + "/" + _ + "?").mkString(view.table + " ", "; ", "")
        } else 
        else 
      List(view.joins, autoJoins, view.joins).filter(_ != null).mkString("; ")
    }
    */
    val where = (filter.map(f =>
      queryColExpression(fieldNameToDef(f._2.Field)) + " " + comparison(f._2.Comparison) +
        " :" + f._1) ++ Option(wherePlus._1).filter(_ != ""))
      .mkString("[", " & ", "]")

    val order =
      if (countAll || sort == null || sort.size == 0) ""
      else sort.map(s => (if (s.Order == "desc" || s.Order == "desc null") "~" else "") +
        "#(" + (fieldNameToDef(s.Field) match {
          case f if f.isExpression && f.expression != null =>
            f.expression
          case f if isRu(f) =>
            "NLSSORT(" + queryColName(f) + ", 'NLS_SORT = RUSSIAN')"
          case f => queryColName(f)
        }) + (if (Option(s.Order).getOrElse("") endsWith " null") " null" else "") + ")").mkString("")

    def limitOffset(query: String) = (if (countAll) (0, 0) else (limit, offset)) match {
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
      val v = f._2.Value
      // TODO describe convertion error (field, table, value, ...)
      f._1 -> (fieldNameToDef(f._2.Field).xsdType.name match {
        case "string" => v
        case "int" => v.toInt
        case "long" => v.toLong
        case "integer" => BigInt(v)
        case "decimal" => BigDecimal(v)
        case "date" => Format.xsdDate.parse(v)
        case "dateTime" => Format.xsdDateTime.parse(v)
        case "boolean" => v match {
          case "true" | "TRUE" => "Y"
          case "false" | "FALSE" => "N"
          case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
        }
        case x => sys.error("Filter value type not supported: " + x)
      })
    }).toMap

    import language.existentials
    val (q, limitOffsetPars) = limitOffset(from + where + cols + order)
    (q, values ++ wherePlus._2 ++ limitOffsetPars.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap)
  }

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)
}
