package querease

import org.tresql.Env
import org.tresql.QueryCompiler._
import org.tresql.{ CacheBase, SimpleCacheBase }

import mojoz.metadata.in.Join
import mojoz.metadata.in.JoinsParser
import mojoz.metadata.ColumnDef
import mojoz.metadata.Type

import scala.collection.immutable.Seq
import scala.util.control.NonFatal

class TresqlJoinsParser(tresqlMetadata: TresqlMetadata) extends JoinsParser {
  val cache: Option[CacheBase[Exp]] = Some(new SimpleCacheBase[Exp](4096))
  def apply(baseTable: String, joins: Seq[String]) = if (joins == null || joins == Nil) List() else {
    val oldMetadata = Env.metadata
    Env.metadata = tresqlMetadata
   try {
    val joinsStr = (Option(baseTable).toList ++ joins).mkString("; ")
    //prefix joins with [] so that compiler knows that it is a join not division operation
    val compileStr = "\\w".r.findFirstIn(joinsStr.substring(0, 1))
      .map(_ => "[]" + joinsStr)
      // otherwise suffix with {*} to avoid BinSelectDef and get proper metadata
      .getOrElse(joinsStr + " {*}")
    val compiledExpr =
      try {
        cache.flatMap(_.get(compileStr)).getOrElse {
          val e = compile(compileStr)
          cache.foreach(_.put(compileStr, e))
          e
        }
      } catch {
        case NonFatal(ex) =>
          throw new RuntimeException("Failed to compile: " + compileStr, ex)
      }
    def exprNotSupportedException(x: AnyRef) = sys.error(
      "Joins can be parsed only from select statement, instead found: " +
      (Option(x).map(_.getClass.getName).orNull + " in " + compileStr))
    def selectDefBase(x: AnyRef): SelectDefBase = x match {
      case s: SelectDefBase => s
      case b: Braces => selectDefBase(b.expr)
      case x => exprNotSupportedException(x)
    }
    selectDefBase(compiledExpr) match {
     case s: SelectDef => s.tables
      .filter { //filter out aliases
        case TableDef(_, Obj(_: TableAlias, _, _, _, _)) => false
        case _ => true
      }.map { t =>
        val alias = t.name
        val table = declaredTable(List(s))(alias).get
        val tableName = // FIXME may clash!
          tresqlMetadata.tableOption(table.name).map(_.name).orNull
        val outerJoin = t.exp.outerJoin == "l" //left outer join
        Join (
          alias = alias,
          table = tableName,
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
     case x => exprNotSupportedException(x)
    }
   } finally {
      Env.metadata = oldMetadata
   }
  }
}
