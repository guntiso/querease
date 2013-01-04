package xsdgen

import org.tresql.QueryParser._

object JoinsParser {
  def aliasToTableMap(joins: String): Map[String, String] = if (joins == null) Map() else {
    parseExp(joins) match {
      case Query(tables, _, _, _, _, _, _, _) => (tables flatMap {
        // primary key join: customer c[c.m_id m, c.f_id f]person  
        case Obj(Ident(name), null, Join(false, Arr(jl), _), _) if (jl.size > 1) => (jl map {
          case Obj(Ident(_), al, _, _) if al != null => al -> name.mkString(".")
          case _ => (null, null)
        }).filter(_._1 != null)
        // normal join: customer c[c.person_id = p.id]person p
        // default join: customer c/person m
        // alias join: customer c/person m;c/person f
        case Obj(Ident(name), alias, _, _) if alias != null => List(alias -> name.mkString("."))
        case _ => Nil
      }).filter(_._1 != null).toMap
      case _ => sys.error("Invalid join: " + joins)
    }
  }
}
