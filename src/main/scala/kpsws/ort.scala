package kpsws

import org.tresql.ORT
import org.tresql.NameMap
import org.tresql.Query
import xsdgen.ElementName
import scala.collection.JavaConversions._
import javax.xml.datatype._
import kpsws.impl._

/** TODO This is duplicated code from file XsdGen.scala */
case class XsdTypeDef(
  name: String,
  table: String,
  fields: Seq[XsdTypeDef])

object ort extends org.tresql.NameMap {

  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance

  def pojoToMap(pojo: Any): Map[String, _] =
    if (pojo == null) Map.empty
    else pojo.getClass.getMethods filter (m =>
      m.getName.startsWith("get") && m.getParameterTypes.size == 0) map (m =>
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
            if (t == classOf[java.lang.Integer])
              m.invoke(pojo, new java.lang.Integer(d.toInt))
            else if (t == classOf[java.lang.Long])
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
          case x: String if t == classOf[java.lang.Boolean] => value match {
            case "Y" | "true" => m.invoke(pojo, java.lang.Boolean.TRUE)
            case "N" | "false" => m.invoke(pojo, java.lang.Boolean.FALSE)
            case null => m.invoke(pojo, java.lang.Boolean.FALSE)
            case x => throw new RuntimeException(
              "No idea how to convert to boolean: \"" + x + "\"")
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

  private def xsdNameToDbName(xsdName: String) = {
    // FIXME DUPLICATE CODE WITH XSDGEN PROJECT
    // FIXME DUPLICATE CLASS ElementName WITH XSDGEN PROJECT
    ElementName.get(xsdName).split("\\-").map(_.toLowerCase match {
      case "user" => "usr"
      case "group" => "grp"
      case "role" => "rle"
      case x => x
    }).mkString("_") match {
      case x if x endsWith "_2" => x.replace("_2", "2") // XXX dirty fix phone_2
      case x => x
    }
  }

  private def xsdValueToDbValue(xsdValue: Any) = xsdValue match {
    case true => "Y"
    case false => "N"
    case x => x
  }

  private def nextId() = Query.unique[Long]("dual{seq.nextval}")

  // TODO derive table name from pojo class and corresponding view def.
  def save(tableName: String, pojo: AnyRef): Long = {
    val propMap = pojoToMap(pojo).map(e =>
      (xsdNameToDbName(e._1), xsdValueToDbValue(e._2)))
    val (id, isNew) = propMap.get("id").filter(_ != null).map(
      _.toString.toLong -> false) getOrElse (nextId(), true)
    if (isNew) ORT.insert(tableName, propMap + ("id" -> id))
    else ORT.update(tableName, propMap)
    id
  }

  def save(tableName: String, pojo: AnyRef, id: Long) =
    ORT.save(tableName,
      pojoToMap(pojo).map(e => (xsdNameToDbName(e._1), e._2)) + ("id" -> id))

  /* -------- Query support methods -------- */

  def query[T](view: XsdTypeDef, pojoClass: Class[T], params: ListRequestType) =
    Query.select(queryString(view, params), (values(params.Filter) ++
        (if (params.Limit > 0 && params.Offset >= 0) Array(params.Offset, params.Limit + params.Offset)
            else Array[String]())): _*).toListRowAsMap.map(mapToPojo(_, pojoClass.newInstance))

  private def queryString(view: XsdTypeDef, params: ListRequestType) = limitOffset(from(view) +
      where(view.name, params.Filter) + cols(view) + sort(view.name, params.Sort),
      params.Offset, params.Limit)

  private def fn(baseView: String, name: String) = name.split("\\.") match {
    case Array(c) => tableName(baseView) + "." + colName(baseView, c)
    case Array(v, c) => tableName(v) + "." + colName(v, c)
  }

  private def cols(view: XsdTypeDef) =
    view.fields.map(f => f.table + "." + f.name).mkString(" {", ", ", "}")

  private def from(view: XsdTypeDef) =
    view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table).mkString("/")

  // FIXME avoid injection - ensure field name can not drop database
  private def where(baseView: String, filter: Array[ListFilterType]) =
    if (filter == null || filter.size == 0) ""
    else filter.map(f =>
      fn(baseView, f.Field) + " " + f.Comparison + " ?").mkString("[", " & ", "]")

  private def values(filter: Array[ListFilterType]) =
    if (filter == null) Array[String]() else filter.map(_.Value)

  // FIXME avoid injection - ensure field name can not drop database
  private def sort(baseView: String, sort: Array[ListSortType]) =
    if (sort == null || sort.size == 0) ""
    else sort.map(s =>
      (if (s.Order == "desc") "~" else "") +
        "#(" + fn(baseView, s.Field) + ")").mkString("")
  
  private def limitOffset(query:String, limit:Int, offset:Int) = if (limit >= 0 && offset > 0) {
    "/(" + query + ") [rownum >= ? & rownum < ?]"
  } else query

  //TODO support limit, offset

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)
}
