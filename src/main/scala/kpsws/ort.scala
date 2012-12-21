package kpsws

import org.tresql.ORT
import org.tresql.NameMap

object ort extends org.tresql.NameMap {

  def pojoToMap(pojo: Any) = pojo.getClass.getMethods filter (m => m.getName.startsWith("get")
      && m.getParameterTypes.size == 0) map (m => m.getName.drop(3) -> m.invoke(pojo)) toMap

  def mapToPojo(map: Map[String, _], pojo: Any) = map foreach (
      t => pojo.getClass.getMethods.filter(m => m.getName == "set" + t._1) match {
    case Array(x) => t._2 match {
      case d: BigDecimal => {
        val t = x.getParameterTypes()(0)
        if (t == classOf[Double] || t == classOf[java.lang.Double]) x.invoke(pojo, d.doubleValue.asInstanceOf[Object])
        else if (t == classOf[java.math.BigDecimal]) x.invoke(pojo, d.bigDecimal)
      }
      case _ => x.invoke(pojo, t._2.asInstanceOf[Object])
    }
    case _ =>
  })
  
  

  def db_ws_name_map(ws: Map[String, _]) = ws.map(t => t._1.toLowerCase -> t._1)

}