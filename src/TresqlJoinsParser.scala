package org.mojoz.querease

import org.tresql.compiling.Compiler
import org.tresql.{ CacheBase, SimpleCacheBase }
import org.tresql.parsing.Braces
import org.tresql.parsing.Exp
import org.tresql.parsing.Obj

import org.mojoz.metadata.in.Join
import org.mojoz.metadata.in.JoinsParser
import org.mojoz.metadata.ColumnDef
import org.mojoz.metadata.TableDef.{ TableDefBase => TableDef }
import org.mojoz.metadata.Type
import org.mojoz.metadata.TypeDef

import scala.collection.immutable.{ Map, Seq }
import scala.language.reflectiveCalls
import scala.util.control.NonFatal

class TresqlJoinsParser(tresqlMetadata: TresqlMetadata) extends JoinsParser {
  trait JoinsParserCompiler extends Compiler {
    val metadata: TresqlMetadata
    def compile(exp: String): Exp
  }
  val dbToMetadata: Map[String, TresqlMetadata] =
    tresqlMetadata.extraDbToMetadata + (tresqlMetadata.db -> tresqlMetadata)
  val dbToCompilerAndCache: Map[String, (JoinsParserCompiler, Option[CacheBase[Exp]])] = tresqlMetadata.dbSet.map { db =>
    val joinsParserCompiler = new JoinsParserCompiler {
      override val metadata = if (db == tresqlMetadata.db) tresqlMetadata else dbToMetadata(db)
      override val extraMetadata = dbToMetadata - db
      override def compile(exp: String): Exp = compile(parseExp(exp))
    }
    val cache: Option[CacheBase[Exp]] = Some(new SimpleCacheBase[Exp](4096))
    db -> (joinsParserCompiler, cache)
  }.toMap
  def apply(db: String, baseTable: String, joins: Seq[String]) = if (joins == null || joins == Nil) List() else {
    val (joinsParserCompiler, cache) = dbToCompilerAndCache(db)
    import TresqlJoinsParser._
    import joinsParserCompiler.{ declaredTable, metadata }
    import joinsParserCompiler.{ SelectDef, SelectDefBase, TableDef, TableAlias, WithSelectDef }
    val (firstNonCteJoinIdx, joinsStr) =
      firstNonCteJoinIdxAndJoinsString(baseTable, joins)
    val compileStr =
      if (firstNonCteJoinIdx != 0)
        // if starts with CTE, leave as is
        joinsStr
      else if (starts_ident_regex.findFirstIn(joinsStr).isDefined)
        // if starts with other ident:
        // prefix joins with [] so that compiler knows that it is a join not division operation
        "[]" + joinsStr
      else
        // otherwise (subquery) suffix with {*} to avoid BinSelectDef and get proper metadata
        joinsStr + " {*}"
    val compiledExpr =
      try {
        cache.flatMap(_.get(compileStr)).getOrElse {
          val e = joinsParserCompiler.compile(compileStr)
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
    val (s, scopes, tables) = selectDefBase(compiledExpr) match {
      case s: SelectDef => (s, List(s), s.tables)
      case w: WithSelectDef => (w, w.exp :: w.withTables.reverse, w.tables)
      case x => exprNotSupportedException(x)
    }
    tables
      .filter { //filter out aliases
        case TableDef(_, Obj(_: TableAlias, _, _, _, _)) => false
        case _ => true
      }.map { t =>
        val alias = t.name
        val table = declaredTable(scopes)(alias)(joinsParserCompiler.EnvMetadata, db = None).get
        val tableName =
          tresqlMetadata.tableOption(table.name, db).map(_.name).orNull
        val outerJoin = t.exp.outerJoin == "l" //left outer join
        Join (
          alias = alias,
          table = tableName,
          isOuterJoin = outerJoin,
          columns = table.cols.map { col =>
            ColumnDef(
              name = col.name,
              type_ = Type(metadata.sql_xsd_type_map(col.sqlType), None, None, None, false),
              nullable = outerJoin || col.nullable,
              dbDefault = null,
              enum_ = Nil,
              comments = null,
              extras = Map.empty
            )
          }
        )
      }
  }
}

object TresqlJoinsParser {
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val ws = """([\h\v]*+(/\*(.|[\h\v])*?\*/)?(//.*+(\n|$))?)+"""
  private val starts_cte = s"^ $ident \\( #? (($ident|\\*)(, ($ident|\\*))*)? \\) \\{".replace(" ", ws)
  private val starts_cte_regex = starts_cte.r
  private val starts_ident = s"^ $ident".replace(" ", ws)
  private val starts_ident_regex = starts_ident.r
  private def firstNonCteJoinIdxAndJoinsString(baseTable: String, joins: Seq[String]) = {
    val firstNonCteJoinIdx = joins.indexWhere(starts_cte_regex.findFirstIn(_).isEmpty)
    val joinsStr = {
      val btl = Option(baseTable).toList
      if (joins.isEmpty)
        btl.mkString("")
      else if (firstNonCteJoinIdx < 0)
        joins.mkString(", ")
      else if (firstNonCteJoinIdx == 0)
        (btl ++ joins).mkString("; ")
      else
        joins.slice(0, firstNonCteJoinIdx).mkString("", ", ", " ") +
          (btl ++ joins.slice(firstNonCteJoinIdx, joins.size)).mkString("; ")
    }
    (firstNonCteJoinIdx, joinsStr)
  }
  private[querease] def joinsString(baseTable: String, joins: Seq[String]): String =
    firstNonCteJoinIdxAndJoinsString(baseTable, joins)._2
  def apply(
    tableDefs: Seq[TableDef[ColumnDef[Type]]],
    typeDefs: collection.immutable.Seq[TypeDef],
    functionSignaturesClass: Class[_]
  ): TresqlJoinsParser = {
    new TresqlJoinsParser(TresqlMetadata(tableDefs, typeDefs, functionSignaturesClass))
  }
}
