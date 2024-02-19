package org.mojoz.querease

import org.mojoz.querease.TresqlMetadata.{CursorsComplexTypePattern, CursorsSchemaName}
import org.tresql.ast.Exp
import org.tresql.parsing.QueryParsers
import org.tresql.{Expr, Macros, Metadata, QueryBuilder, ast, metadata}

import scala.collection.mutable.{ArrayBuffer => AB, Map => MM}
import scala.util.Try
import scala.util.matching.Regex

class QuereaseMacros extends Macros {

  val CursorRowNrColName = "__row_nr"
  val CursorRowNrRefColName = "__row_nr_ref"
  /** Postgres db max identifier length */
  val MaxCursorNameLength = 63

  private case class Cursor[T](cols: List[String], values: AB[T])
  private def isComplexType(tn: String) = CursorsComplexTypePattern.pattern.matcher(tn).matches()
  private def tresqlType(metadata: Metadata, scalaType: String) = metadata.scala_xsd_type_map(scalaType)
  private def tableCols(table: metadata.Table) = table.cols.filter(c => !isComplexType(c.scalaType.name))
  private def tableComplexCols(table: metadata.Table) = table.cols.collect {
    case c if isComplexType(c.scalaType.name) =>
      val CursorsComplexTypePattern(t) = c.scalaType.name : @unchecked
      (c.name, t)
  }
  private def cursorTable(metadata: Metadata, name: String) = metadata.table(s"$CursorsSchemaName.$name")
  private def addEmptyCursor[T](name: String, tableName: String, curs: MM[String, Cursor[T]],
                        emptyRowFun: org.tresql.metadata.Table => T, metadata: org.tresql.Metadata): Unit = {
    if (!curs.contains(name) && name.length <= MaxCursorNameLength) {
      val table = cursorTable(metadata, tableName)
      val (table_cols, child_cols) =
        table.cols.partition(c => !isComplexType(c.scalaType.name))
      curs += (name -> Cursor(
        CursorRowNrColName :: CursorRowNrRefColName :: table_cols.map(_.name),
        AB(emptyRowFun(table))
      ))
      child_cols.foreach { c =>
        val n = c.name
        val CursorsComplexTypePattern(vn) = c.scalaType.name : @unchecked
        addEmptyCursor(s"${name}_$n", vn, curs, emptyRowFun, metadata)
      }
    }
  }

  def build_cursors(b: QueryBuilder, view: Expr): Expr = build_cursors(b, view, null)

  def build_cursors(b: QueryBuilder, view: Expr, dataVariable: QueryBuilder#VarExpr): Expr = {
    def constVal(v: Any, typ: String = null) =
      if (typ == null) b.ConstExpr(v) else b.CastExpr(b.ConstExpr(v), typ)
    def emptyVal(typ: String) = constVal(null, typ)
    val tresql_type = tresqlType(b.env.metadata, _)
    def withTables(cursors: MM[String, Cursor[Expr]]) =
      cursors.foldLeft(List[b.WithTableExpr]()) { case (res, (name, curs)) =>
        withTable(name, curs) :: res
      }.reverse
    def withTable(name: String, c: Cursor[Expr]) = b.WithTableExpr(name, c.cols, false,
      c.values.tail.foldLeft(c.values.head) {b.BinExpr("++", _, _)})
    def row(path: List[String], table: metadata.Table, values: Map[String, Any], isEmpty: Boolean) = {
      val id = constVal(path.headOption.flatMap(i => Try(i.toInt).toOption).getOrElse(0))
      val parent_id =
        if (path.isEmpty) emptyVal("int")
        else constVal(path.tail.find(i => Try(i.toInt).toOption.nonEmpty).orNull, "int")
      def colExpr(name: String, typ: String) = {
        if (!isEmpty && values.contains(name)) {
          val rp = (name :: path).reverse
          b.CastExpr(b.VarExpr(rp.head, rp.tail, false), tresql_type(typ))
        } else {
          emptyVal(tresql_type(typ))
        }
      }
      val cols = (id :: parent_id ::
        tableCols(table).map(c => colExpr(c.name, c.scalaType.name))).map(b.ColExpr(_, null))
      val filter = if (isEmpty) b.BinExpr("=", constVal(1), constVal(0)) else null
      b.SelectExpr(List(
        b.Table(b.ConstExpr(ast.Null), null, b.TableJoin(false, null, true, null), null, false, null)),
        filter, b.ColsExpr(cols, false, false, false),
        false, null, null, null, null, Map(), None
      )
    }
    def addRow(name: String, path: List[String], table: metadata.Table, values: Map[String, Any],
               curs: MM[String, Cursor[Expr]]): Unit =
      curs.get(name).map { c =>
        c.values += row(path, table, values, false)
      }.getOrElse {
        val cols = CursorRowNrColName :: CursorRowNrRefColName :: tableCols(table).map(_.name)
        val vals = row(path, table, values, false)
        curs += (name -> Cursor(cols, AB(vals)))
        () // for scala 2.12 compiler
      }

    def traverseData(tableName: String, path: List[String], data: Any): MM[String, Cursor[Expr]] = {
      val cursors = MM[String, Cursor[Expr]]()
      val cursTable = cursorTable(b.env.metadata, _)

      def traverseMap(cursorName: String,
                      table: metadata.Table,
                      path: List[String],
                      values: Map[String, Any]): Unit = {

        def nonEmptyName(n: String) = n != null && n.nonEmpty

        if (nonEmptyName(cursorName)) // do not make cursor for top level table to save resources
          addRow(cursorName, path, table, values, cursors)

        tableComplexCols(table).foreach { case (col_name, table_name) =>
          val ch_name = (if (nonEmptyName(cursorName)) cursorName + "_" else "") + col_name
          def empty_curs =
            addEmptyCursor(ch_name, table_name, cursors, row(Nil, _, Map(), true), b.env.metadata)

          values.get(col_name).map {
            case lookup: Map[String@unchecked, _] =>
              traverseMap(ch_name, cursTable(table_name), col_name :: path, lookup)
            case children: Iterable[_] =>
              val new_path = col_name :: path
              val succ = children.zipWithIndex.foldLeft(false) {
                case (_, (m: Map[String@unchecked, _], i)) =>
                  traverseMap(ch_name, cursTable(table_name), i.toString :: new_path, m)
                  true
                case (r, _) => r
              }
              if (!succ) empty_curs
            case _ => empty_curs
          }.getOrElse(empty_curs)
        }
      }

      val table = cursTable(tableName)
      data match {
        case m: Map[String@unchecked, _] =>
          traverseMap("", table, path, m)
        case i: Iterable[_] =>
          i.zipWithIndex.foreach {
            case (m: Map[String@unchecked, _], i) =>
              traverseMap(path.lastOption.getOrElse(tableName), table, i.toString :: path, m)
            case _ =>
          }
        case _ => traverseMap("", table, Nil, Map())
      }
      cursors
    }

    lazy val transformer: PartialFunction[Expr, Expr] = {
      case s@b.SelectExpr(
        b.Table(_, _,
        b.TableJoin(_, b.ArrExpr(List(b.FunExpr("build_cursors", _, _, _, _))), _, _)
        , _, _, _
        ) :: _,
        _, _, _, _, _, _, _, _, _
      ) =>
        val (data, initPath) =
          if (dataVariable == null) (b.env.params, Nil)
          else (b.env(dataVariable.fullName), dataVariable.name :: dataVariable.members)
        val name = view match {
          case v: b.ConstExpr => String.valueOf(v())
          case v: b.IdentExpr => v.name.mkString(".")
          case x => sys.error(s"Error invoking build_cursors macro. " +
            s"View name parameter must be string literal or identifier, found: $x of class (${x.getClass})")
        }
        val cursors = traverseData(name, initPath, data)
        b.WithSelectExpr(withTables(cursors), s)

      case b.WithSelectExpr(tables, q) =>
        val tr_tables = tables.map(b.transform(_, transformer)).asInstanceOf[List[b.WithTableExpr]]
        b.transform(q, transformer) match {
          case w: b.WithSelectExpr => b.WithSelectExpr(w.tables ++ tr_tables, w.query)
          case s: b.SelectExpr => b.WithSelectExpr(tr_tables, s)
          case x => x
        }
    }

    b.TransformerExpr(transformer)
  }

  def build_cursors(p: QueryParsers, view: Exp): Exp = build_cursors(p, view, null)

  def build_cursors(p: QueryParsers, view: Exp, data: ast.Variable): Exp = {
    val md = p match {
      case c: org.tresql.compiling.Compiler => c.metadata
      case x => sys.error(s"Parser must be of type 'org.tresql.compiling.Compiler' to execute 'build_cursors' macro." +
        s" Instead found type: ${x.getClass.getName}")
    }
    def emptyRow(table: metadata.Table) = {
      val vals = ast.Col(ast.IntConst(0), null) :: ast.Col(ast.Null, null) :: tableCols(table).map { c =>
        ast.Col(ast.Cast(ast.Null, tresqlType(md, c.scalaType.name)), null)
      }
      ast.Query(
        List(ast.Obj(ast.Null)),
        ast.Filters(List(ast.Arr(List(ast.BinOp("=", ast.IntConst(1), ast.IntConst(0)))))),
        ast.Cols(false, vals)
      )
    }
    def withQuery(q: Exp) = {
      def withTable(n: String, c: Cursor[Exp]) = ast.WithTable(n, c.cols, false, c.values.head)

      val viewName = view match {
        case ast.Obj(ast.Ident(n), _, _, _, _) => n.mkString(".")
        case ast.StringConst(n) => n
        case x => sys.error(s"Error invoking build_cursors macro. " +
          s"View name parameter must be string literal or identifier, found: $x of class (${x.getClass})")
      }
      val cs = MM[String, Cursor[Exp]]()
      Option(data).map(v => (v.variable :: v.members).last).map(cursorName => List(cursorName -> viewName))
        .getOrElse(tableComplexCols(cursorTable(md, viewName)))
        .foreach { case (cn, vn) => addEmptyCursor(cn, vn, cs, emptyRow, md) }

      ast.With(cs.map { case (n, c) => withTable(n, c) }.toList, q)
    }

    lazy val transformer: p.Transformer = p.transformer {
      case q@ast.Query(
        ol@ast.Obj(_, _, ast.Join(_,
        ast.Arr(List(ast.Fun("build_cursors", _, _, _, _))), _
        ), _, _) :: os, _, _, _, _, _, _
      ) =>
        withQuery(q.copy(tables = ol.head.copy(join = null) :: os))
      case o@ast.Obj(_, _, ast.Join(_,
        ast.Arr(List(ast.Fun("build_cursors", _, _, _, _))), _
        ), _, _
      ) =>
        withQuery(o.copy(join = null))
      case ast.With(tables, query) =>
        val tr_tables = (tables map transformer).asInstanceOf[List[ast.WithTable]]
        transformer(query) match {
          case w: ast.With => ast.With(w.tables ++ tr_tables, w.query) //put bind var cursor tables first
          case q: Exp => ast.With(tr_tables, q)
        }
    }/*.andThen {
      case e =>
        println(s"Compiling:\n${e.tresql}\n\n")
        e
    }*/
    ast.TransformerExp(transformer)
  }
}
