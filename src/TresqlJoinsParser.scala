package querease

import org.tresql.QueryCompiler._
import org.tresql.QueryCompiler.{ Join => QCJoin }

import mojoz.metadata.in.Join
import mojoz.metadata.in.JoinsParser
import mojoz.metadata.ColumnDef
import mojoz.metadata.Type

object TresqlJoinsParser extends JoinsParser {
  def apply(baseTable: String, joins: Seq[String]) = if (joins == null || joins == Nil) List() else {
    val joinsStr = joins.mkString("; ")
    //prefix joins with [] so that compiler knows that it is a join not division operation
    compile("\\w".r.findFirstIn(joinsStr.substring(0, 1)).map(x=> "[]").getOrElse("") + joinsStr) match {
      case s: SelectDef => s.tables.filter { //filter out aliases
        case TableDef(_, Obj(_: TableAlias, _, _, _, _)) => false
        case _ => true
      }.map { t =>
        val alias = t.name
        val table = declaredTable(List(s))(alias).get
        val outerJoin = t.exp.outerJoin == "l" //left outer join
        Join (
          alias = alias,
          table = table.name,
          columns = table.cols.map { col =>
            ColumnDef(
              name = col.name,
              type_ = Type(metadata.sql_xsd_type_map(col.sqlType), None, None, None, false),
              nullable = outerJoin || col.nullable,
              dbDefault = null,
              enum = Nil,
              comments = null,
              extras = Map.empty
            )
          }
        )
      }
      case x => sys.error(s"Joins can be parsed only from select statement, instead found this: $x")
    }
  }
}
