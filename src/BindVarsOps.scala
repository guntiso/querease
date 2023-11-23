package org.mojoz.querease

import org.mojoz.metadata.{FieldDef, ViewDef}
import org.mojoz.querease.QuereaseMetadata.BindVarCursorsFunctionName
import org.tresql.ast.{Arr, Exp, Fun, Ident, Join, Obj, StringConst, Variable, With, Query => PQuery}
import org.tresql.parsing.QueryParsers
import org.tresql.{MissingBindVariableException, Query, Resources, SingleValueResult}

import scala.collection.mutable.{ArrayBuffer => AB, Map => MM}
import scala.util.Try

trait BindVarsOps {
  this: QuereaseMetadata with QuereaseExpressions with QuereaseResolvers
  with QueryStringBuilder with FilterTransformer =>

  protected val BindVarsCursorRowNrColName = "__row_nr"
  protected val BindVarsCursorRowNrRefColName = "__row_nr_ref"

  def cursorsFromViewBindVars(initPath: List[String], data: Any, view: ViewDef): String = {
    case class Cursor(cols: Seq[String], values: AB[Seq[String]], isEmpty: Boolean)
    type BindVarsWithEmptyCursors = MM[String, Cursor]
    val cursors: BindVarsWithEmptyCursors = MM()

    def emptyVal(tn: String) = "null" + typeCast(tn)

    def typeCast(tn: String) =
      "::" + tresqlMetadata.simpleTypeNameToXsdSimpleTypeName.getOrElse(tn, tn)

    def idFields(addParentField: Boolean) =
      BindVarsCursorRowNrColName ::
        (if (addParentField) List(BindVarsCursorRowNrRefColName) else Nil)

    def addEmptyCursor(cName: String, vd: ViewDef, addParentField: Boolean,
                       visited: Set[String]): Unit = {
      val (complex_fields, primitive_fields) = vd.fields.partition(_.type_.isComplexType)
      val id_fields = idFields(addParentField)
      if (!cursors.contains(cName)) {
        cursors += (cName -> Cursor(
          id_fields ++ primitive_fields.map(_.fieldName),
          AB(id_fields.map(_ => "null::long") ++ primitive_fields.map(f => emptyVal(f.type_.name))),
          true))
      }
      complex_fields.foreach { f =>
        val tn = f.type_.name
        if (!visited(tn))
          addEmptyCursor(cName + "_" + f.fieldName, viewDef(tn), true, visited + tn)
      }
    }

    def addRow(cName: String, cols: Seq[FieldDef], path: List[String], row: Map[String, Any]): Unit = {
      val parent_id = if (path.isEmpty) None else path.tail.find(i => Try(i.toInt).toOption.nonEmpty)
      val id = path.headOption.filter(i => Try(i.toInt).isSuccess).getOrElse("0")
      def addVals(rows: AB[Seq[String]]) = {
        val bind_var_prefix = path.reverse.mkString(":", ".", ".")
        rows += ((id :: parent_id.toList) ++ cols.map { col =>
          val n = col.fieldName
          (if (row.contains(n)) bind_var_prefix + n else "null") + typeCast(col.type_.name)
        })
        rows
      }
      cursors.get(cName).map { c =>
        if (c.isEmpty) cursors += (cName -> c.copy(values = addVals(AB()), isEmpty = false))
        else addVals(c.values)
        () // for scala 2.12 compilation
      }.getOrElse {
        //create new cursor
        cursors += (cName -> Cursor(
          idFields(parent_id.nonEmpty) ++ cols.map(_.fieldName), addVals(AB()), false))
        () // for scala 2.12 compilation
      }
    }

    def traverseData(vd: ViewDef, path: List[String], values: Any): Unit = {
      def traverseMap(cName: String, vd: ViewDef, path: List[String], values: Map[String, Any]): Unit = {
        val (complex_fields, primitive_fields) = vd.fields.partition(_.type_.isComplexType)
        if (cName != null && cName.nonEmpty) { //do not make cursor for top level view
          addRow(cName, primitive_fields, path, values)
        }
        complex_fields.foreach { f =>
          val fn = f.fieldName
          val ch_view = viewDef(f.type_.name)
          val ch_cursor = (if (cName == null || cName.isEmpty) "" else cName + "_") + fn

          def emptyCurs =
            addEmptyCursor(ch_cursor, ch_view, path.exists(p => Try(p.toInt).toOption.nonEmpty), Set())

          values.get(fn).map {
            case lookup: Map[String@unchecked, _] =>
              traverseMap(ch_cursor, ch_view, fn :: path, lookup)
            case children: Iterable[_] =>
              val new_path = fn :: path
              val succ = children.zipWithIndex.foldLeft(false) {
                case (_, (m: Map[String@unchecked, _], i)) =>
                  traverseMap(ch_cursor, ch_view, i.toString :: new_path, m)
                  true
                case (r, _) => r
              }
              if (!succ) emptyCurs
            case _ => emptyCurs
          }.getOrElse(emptyCurs)
        }
      }

      values match {
        case m: Map[String@unchecked, _] =>
          traverseMap("", vd, path, m)
        case i: Iterable[_] =>
          i.zipWithIndex.foreach {
            case (m: Map[String@unchecked, _], i) =>
              traverseMap(path.lastOption.getOrElse(vd.name), vd, i.toString :: path, m)
            case _ =>
          }
        case _ => traverseMap("", vd, Nil, Map())
      }
    }

    traverseData(view, initPath, data)
    cursors.map { case (cName, Cursor(cols, values, isEmpty)) =>
      cName + cols.mkString("(#", ", ", ")") +
        values.map(_.mkString(if (isEmpty) "null[1 = 0]{" else "{", ", ", "}"))
          .mkString(" {", " ++ ", "}")
    }.mkString(", ")
  }

  def extractDataForVars(data: Map[String, Any], vars: List[String]): Map[String, Any] = {
    implicit val env = new Resources {}.withParams(data)
    vars match {
      case Nil => data
      case l => l.foldLeft(Map[String, Any]()) { (r, v) =>
        Query(v) match {
          case SingleValueResult(value) => value match {
            case m: Map[String, _]@unchecked => r ++ m
            case x: Any =>
              r + (parser.parseWithParser(parser.variable)(v).variable -> x)
          }
          case x => sys.error(s"Cannot extract variable value from result: $x")
        }
      }
    }
  }

  def emptyData(view: ViewDef): Map[String, Any] = {
    def calc(vd: ViewDef, visited: Set[String]): Map[String, Any] = {
      vd.fields.map { f =>
        f.fieldName -> {
          if (f.type_.isComplexType) {
            val n = f.type_.name
            if (visited(n)) null
            else {
              val ed = calc(nameToViewDef(n), visited + n)
              if (f.isCollection) List(ed) else ed
            }
          } else null
        }
      }.toMap
    }
    calc(view, Set())
  }

  def maybeExpandWithBindVarsCursorsForView(
    tresql: String,
    vars: Map[String, Any],
  )(
    view: ViewDef,
    qp: QueryParsers,
  ): String = {
    if (tresql.indexOf(BindVarCursorsFunctionName) == -1) tresql
    else {
      def bindVarsCursorsCreator: qp.Transformer = {
        qp.transformer {
          case q@PQuery(
          ol@Obj(_, _, Join(_,
          Arr(List(Fun(BindVarCursorsFunctionName, pars, _, _, _))), _
          ), _, _) :: os, _, _, _, _, _, _) =>
            def vals(v: Variable, vn: String) =
              Try(Query.buildFromAst(v)(new Resources {}.withParams(vars))()).recover {
                //during compilation may bind variable cannot be resolved, so use list of view empty data
                case _: MissingBindVariableException => List(emptyData(viewDef(vn)))
              }.get
            val (parsPath, vd, data) = pars match {
              case Nil => (Nil, view, vars)
              case List(vr@Variable(v, p, false), Obj(Ident(List(view_name)), null, null, null, false)) =>
                (v :: p, viewDef(view_name), vals(vr, view_name))
              case List(vr@Variable(v, p, false), StringConst(view_name)) =>
                (v :: p, viewDef(view_name), vals(vr, view_name))
              case x => sys.error(s"$BindVarCursorsFunctionName function takes no parameters or " +
                s"2 parameters (data_variable, view_name), instead got (${x.map(_.tresql).mkString(", ")})")
            }
            val nq =  q.copy(tables = ol.head.copy(join = null) :: os)
            qp.parseExp(cursorsFromViewBindVars(parsPath, data, vd)) match {
              case With(tables, _) => With(tables, nq)
              case _ => nq
            }
          case With(tables, query) => bindVarsCursorsCreator(query) match {
            case w: With => With(w.tables ++ tables, w.query) //put bind var cursor tables first
            case q: Exp => With(tables, q)
          }
        }
      }
      bindVarsCursorsCreator(qp.parseExp(tresql)).tresql
    }
  }
}
