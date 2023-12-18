package org.mojoz.querease

import org.mojoz.querease.TresqlMetadata.{CursorsComplexTypePattern, CursorsSchemaName}
import org.tresql.ast.Exp
import org.tresql.parsing.QueryParsers
import org.tresql.{Expr, Macros, QueryBuilder, ast, metadata}

import scala.collection.mutable.{ArrayBuffer => AB, Map => MM}
import scala.util.Try
import scala.util.matching.Regex

class QuereaseMacros extends Macros {

  val CursorRowNrColName = "__row_nr"
  val CursorRowNrRefColName = "__row_nr_ref"

  def build_cursors(b: QueryBuilder, view: Expr): Expr = build_cursors(b, view, null)

  def build_cursors(b: QueryBuilder, view: Expr, dataVariable: QueryBuilder#VarExpr): Expr = {
    case class Cursor(cols: List[String], values: AB[Expr])

    def constVal(v: Any, typ: String = null) =
      if (typ == null) b.ConstExpr(v) else b.CastExpr(b.ConstExpr(v), typ)
    def emptyVal(typ: String) = constVal(null, typ)
    def tresqlType(scalaType: String) = b.env.metadata.scala_xsd_type_map(scalaType)
    def cursorTable(name: String) = b.env.metadata.table(s"$CursorsSchemaName.$name")
    def isComplexType(tn: String) = CursorsComplexTypePattern.pattern.matcher(tn).matches()
    def tableCols(table: metadata.Table) =
      table.cols.filter(c => !isComplexType(c.scalaType.name))

    def withTables(cursors: MM[String, Cursor]) =
      cursors.foldLeft(List[b.WithTableExpr]()) { case (res, (name, curs)) =>
        withTable(name, curs) :: res
      }.reverse
    def withTable(name: String, c: Cursor) = b.WithTableExpr(name, c.cols, false,
      c.values.tail.foldLeft(c.values.head) {b.BinExpr("++", _, _)})
    def row(path: List[String], table: metadata.Table, values: Map[String, Any], isEmpty: Boolean) = {
      val id = constVal(path.headOption.flatMap(i => Try(i.toInt).toOption).getOrElse(0))
      val parent_id =
        if (path.isEmpty) emptyVal("int")
        else constVal(path.tail.find(i => Try(i.toInt).toOption.nonEmpty).orNull, "int")
      def colExpr(name: String, typ: String) = {
        if (!isEmpty && values.contains(name)) {
          val rp = (name :: path).reverse
          b.CastExpr(b.VarExpr(rp.head, rp.tail, false), tresqlType(typ))
        } else {
          emptyVal(tresqlType(typ))
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
               curs: MM[String, Cursor]): Unit =
      curs.get(name).map { c =>
        c.values += row(path, table, values, false)
      }.getOrElse {
        val cols = CursorRowNrColName :: CursorRowNrRefColName :: tableCols(table).map(_.name)
        val vals = row(path, table, values, false)
        curs += (name -> Cursor(cols, AB(vals)))
        () // for scala 2.12 compiler
      }

    def addEmptyCursor(name: String, table: metadata.Table, curs: MM[String, Cursor]): Unit = {
      val SingleRep = new Regex(s"$name(_$name){2}")
      val MultiRep = new Regex(s"$name")
      if (!curs.contains(name) &&
        // check name has not too much repetitions in case of recursive references to avoid stack overflow
        SingleRep.findFirstMatchIn(name).isEmpty && MultiRep.findAllIn(name).toList.size < 4) {

        val (table_cols, child_cols) =
          table.cols.partition(c => !isComplexType(c.scalaType.name))
        curs += (name -> Cursor(
          CursorRowNrColName :: CursorRowNrRefColName :: table_cols.map(_.name),
          AB(row(Nil, table, Map(), true))
        ))
        child_cols.foreach { c =>
          val n = c.name
          val CursorsComplexTypePattern(vn) = c.scalaType.name
          addEmptyCursor(s"${name}_$n", cursorTable(vn), curs)
        }
      }
    }

    def traverseData(tableName: String, path: List[String], data: Any): MM[String, Cursor] = {
      val cursors = MM[String, Cursor]()
      def traverseMap(cursorName: String,
                      table: metadata.Table,
                      path: List[String],
                      values: Map[String, Any]): Unit = {

        def nonEmptyName(n: String) = n != null && n.nonEmpty

        if (nonEmptyName(cursorName)) // do not make cursor for top level table to save resources
          addRow(cursorName, path, table, values, cursors)

        table.cols.collect {
          case c if isComplexType(c.scalaType.name) =>
            val CursorsComplexTypePattern(t) = c.scalaType.name
            (c.name, t)
        }.foreach { case (col_name, table_name) =>
          val ch_table = cursorTable(table_name)
          val ch_name = (if (nonEmptyName(cursorName)) cursorName + "_" else "") + col_name
          def empty_curs = addEmptyCursor(ch_name, ch_table, cursors)

          values.get(col_name).map {
            case lookup: Map[String@unchecked, _] =>
              traverseMap(ch_name, ch_table, col_name :: path, lookup)
            case children: Iterable[_] =>
              val new_path = col_name :: path
              val succ = children.zipWithIndex.foldLeft(false) {
                case (_, (m: Map[String@unchecked, _], i)) =>
                  traverseMap(ch_name, ch_table, i.toString :: new_path, m)
                  true
                case (r, _) => r
              }
              if (!succ) empty_curs
            case _ => empty_curs
          }.getOrElse(empty_curs)
        }
      }

      val table = cursorTable(tableName)
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
          case v: b.VarExpr => String.valueOf(v())
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

  def build_cursors(p: QueryParsers, view: Exp): Exp = {
    null
  }

  def build_cursors(p: QueryParsers, view: Exp, data: Any): Exp = {
    null
  }
}
