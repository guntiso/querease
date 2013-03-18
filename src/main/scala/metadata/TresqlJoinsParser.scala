package metadata

import org.tresql.QueryParser.{Join => QPJoin, _}

case class Join(alias: String, name: String, nullable: Boolean)

object JoinsParser {
  def apply(joins: String):List[(String, String)] = if (joins == null) List() else {
    var currentJoin: Join = null
    parseExp(joins) match {
      case Query(tables, _, _, _, _, _, _, _) => (tables flatMap {
        case Obj(Ident(name), null, QPJoin(false, Arr(jl), _), _) => (jl map {
          // primary key join: customer c[c.m_id m, c.f_id f]person
          case Obj(Ident(_), al, _, _) => al -> name.mkString(".")
          // normal join
          case _ => (null, name.mkString(".")) 
        })
        // normal join: customer c[c.person_id = p.id]person p
        // default join: customer c/person m
        // alias join: customer c/person m;c/person f
        case Obj(Ident(name), alias, _, _) => List(alias -> name.mkString("."))
        case _ => Nil
      })
      case _ => sys.error("Invalid join: " + joins)
    }
  }
  def aliases(joins:String) = apply(joins).filter(_._1 != null).toMap
}
