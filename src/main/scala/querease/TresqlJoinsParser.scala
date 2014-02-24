package querease

import org.tresql.QueryParser._
import org.tresql.QueryParser.{ Join => QPJoin }

import metadata.Join
import metadata.JoinsParser

trait TresqlJoinsParser extends JoinsParser {
  override def parseJoins(baseTable: String, joins: String) = if (joins == null) List() else {
    var currentJoin: Join = null
    var joinMap: Map[String, Join] = Map()
    def fillJoinMap(join: Join) {
      joinMap += Option(join.alias).getOrElse(join.table) -> join
    }
    def nullable(b: Boolean) = (currentJoin.nullable, b) match {
      case (Right(nullable), n) => Right(nullable || n)
      case (Left(nullable), true) => Right(true)
      case (x, false) => x
    }
    //prefix joins with [] so that parser knows that it is a join not division operation
    parseExp("\\w".r.findFirstIn(joins.substring(0, 1)).map(x=> "[]").getOrElse("") + joins) match {
      case Query(tables, _, _, _, _, _, _, _) => (tables.foldLeft(List[Join]()) { (joins, j) =>
        j match {
          //base table
          case Obj(Ident(name), alias, _, outerJoin, _) if joins.size == 0 =>
            val n = name.mkString(".")
            currentJoin = Join(alias, n, if (n == baseTable) Right(outerJoin != null) else Left(n))
            fillJoinMap(currentJoin)
            currentJoin :: joins
          //foreign key alias join or normal join without alias
          case Obj(Ident(name), null, QPJoin(false, Arr(jl), false), oj1, _) => jl.foldLeft(joins) {
            (joins, j) =>
              j match {
                // foreign key alias join: customer c[c.m_id m, c.f_id f]person
                case Obj(Ident(_), alias, _, oj2, _) =>
                  val join = Join(alias, name.mkString("."), nullable(oj1 != null || oj2 != null))
                  fillJoinMap(join)
                  join :: joins
                // normal join
                case _ =>
                  val join = Join(null, name.mkString("."), nullable(oj1 != null))
                  fillJoinMap(join)
                  currentJoin = join
                  join :: joins
              }
          }
          //alias join i.e. no join
          case Obj(Ident(name), null, QPJoin(false, null, true), null, _) =>
            val n = name.mkString(".")
            currentJoin = joinMap.getOrElse(n, Join(null, n, Left(n)))
            joins
          // normal join with alias: customer c[c.person_id = p.id]person p
          // default join: customer c/person m
          case Obj(Ident(name), alias, _, outerJoin, _) =>
            val join = Join(alias, name.mkString("."), nullable(outerJoin != null))
            fillJoinMap(join)
            currentJoin = join
            join :: joins
          case _ => joins
        }
      }).reverse
      case _ => sys.error("Invalid join: " + joins)
    }
  }
}
