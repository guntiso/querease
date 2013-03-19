package metadata

import org.tresql.QueryParser.{ Join => QPJoin, _ }

case class Join(alias: String, name: String, nullable: Boolean)

object JoinsParser {
  def apply(joins: String): List[Join] = if (joins == null) List() else {
    var currentJoin: Join = null
    var joinMap: Map[String, Join] = Map()
    def fillJoinMap(join: Join) {
      joinMap += Option(join.alias).getOrElse(join.name) -> join
    }
    parseExp(joins) match {
      case Query(tables, _, _, _, _, _, _, _) => (tables.foldLeft(List[Join]()) { (joins, j) =>
        j match {
          //base table
          case Obj(Ident(name), alias, null, outerJoin) => {
            currentJoin = Join(alias, name.mkString("."), outerJoin != null)
            fillJoinMap(currentJoin)
            currentJoin :: joins
          }
          //foreign key alias join or normal join without alias
          case Obj(Ident(name), null, QPJoin(false, Arr(jl), false), oj1) => jl.foldLeft(joins) {
            (joins, j) =>
              j match {
                // foreign key alias join: customer c[c.m_id m, c.f_id f]person
                case Obj(Ident(_), alias, _, oj2) => {
                  val join = Join(alias, name.mkString("."),
                    currentJoin.nullable || oj1 != null || oj2 != null)
                  fillJoinMap(join)
                  join :: joins
                }
                // normal join
                case _ => {
                  val join = Join(null, name.mkString("."), currentJoin.nullable || oj1 != null)
                  fillJoinMap(join)
                  currentJoin = join
                  join :: joins
                }
              }
          }
          //alias join i.e. no join
          case Obj(Ident(name), null, QPJoin(false, null, true), null) => {
            currentJoin = joinMap(name.mkString("."))
            joins
          }
          // normal join with alias: customer c[c.person_id = p.id]person p
          // default join: customer c/person m
          case Obj(Ident(name), alias, _, outerJoin) => {
            val join = Join(alias, name.mkString("."), currentJoin.nullable || outerJoin != null)
            fillJoinMap(join)
            currentJoin = join
            join :: joins
          }
          case _ => joins
        }
      }).reverse
      case _ => sys.error("Invalid join: " + joins)
    }
  }
  def aliases(joins: String) = apply(joins).filter(_.alias != null).map(j => j.alias -> j.name).toMap
}
