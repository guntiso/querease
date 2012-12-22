package kpsws

import org.tresql.ORT
import org.tresql.NameMap
import xsdgen.ElementName

import scala.collection.JavaConversions._
import javax.xml.datatype._

object ort extends org.tresql.NameMap {
  
  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance
    
  def pojoToMap(pojo: Any):Map[String, _] = pojo.getClass.getMethods filter (m=> m.getName.startsWith("get")
      && m.getParameterTypes.size == 0) map (m => m.getName.drop(3) -> (m.invoke(pojo) match {
        case x if (isPrimitive(x)) => x
        case l:Seq[_] => l map pojoToMap
        case l:Array[_] => l map pojoToMap
        case l:java.util.Collection[_] => l map pojoToMap
        case x => pojoToMap(x)
      })) toMap

  def mapToPojo(map: Map[String, _], pojo: Any):Any = {
    pojo.getClass.getMethods.filter(m => m.getName.startsWith("set") &&
      m.getParameterTypes.size == 1) foreach { m =>
      val propName = m.getName.drop(3) //property name
      val propClass = propToClassName(propName) //find class name in the case value is map or list i.e. not primitive object
      val t = m.getParameterTypes()(0)
      map.get(propName).map {
        _ match {
          //this may require tuning since it is difficult with java reflection to determine compatibility
          //between map entry value type and setter parameter type
          //hopefully scala reflection will help with this
          case d: BigDecimal => {
            if (t == classOf[Double] || t == classOf[java.lang.Double]) m.invoke(pojo,
              d.doubleValue.asInstanceOf[Object])
            else if (t == classOf[java.math.BigDecimal]) m.invoke(pojo, d.bigDecimal)
            else if (t == classOf[java.math.BigInteger]) m.invoke(pojo, d.bigDecimal.unscaledValue)
            else m.invoke(pojo, d)
          }
          case x if (t == classOf[java.math.BigInteger] && x != null) => m.invoke(pojo,
            new java.math.BigInteger(x.toString))
          case x:java.util.Date if (t == classOf[XMLGregorianCalendar]) => {
            val gc = new java.util.GregorianCalendar()
            gc.setTime(x)
            m.invoke(pojo, XML_DATATYPE_FACTORY.newXMLGregorianCalendar(gc))
          }
          case inMap: Map[String, _] => mapToPojo(inMap, Class.forName(propClass).newInstance())
          //may be exact collection which is used in xsd generated pojos must be used?
          case Seq() if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => t.newInstance
          case s:Seq[Map[String, _]] if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => {
            val col:java.util.Collection[_] = s.map(mapToPojo(_, Class.forName(propClass).newInstance()))
            col
          }
          case x => m.invoke(pojo, x.asInstanceOf[Object])
        }
      }
    }
    pojo
  }
  
  private def isPrimitive[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null || (x match {
    case _:java.lang.Number | _:java.lang.Boolean | _:java.util.Date | _:XMLGregorianCalendar => true
    case _ => false
  })
  private def propToClassName(prop:String) = if (prop.endsWith("List")) prop.dropRight(4) else prop
  
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

  def save(tableName: String, pojo: AnyRef, id: Long) =
    ORT.save(tableName,
      pojoToMap(pojo).map(e => (xsdNameToDbName(e._1), e._2)) + ("id" -> id))

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)

}