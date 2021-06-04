package org.mojoz.querease

import org.tresql.{Query, Resources, SingleValueResult}
import scala.collection.mutable.{Map => MM, ArrayBuffer => AB}

import scala.util.Try

trait BindVarsOps { this: Querease =>

  protected val BindVarsCursorRowNrColName = "__row_nr"
  protected val BindVarsCursorRowNrRefColName = "__row_nr_ref"

  def cursorsFromViewBindVars(data: Map[String, Any], view: ViewDef): String = {
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
          id_fields ++ primitive_fields.map(_.name),
          AB(id_fields.map(_ => "null::long") ++ primitive_fields.map(f => emptyVal(f.type_.name))),
          true))
      }
      complex_fields.foreach { f =>
        val tn = f.type_.name
        if (!visited(tn))
          addEmptyCursor(cName + "_" + f.name, viewDef(tn), true, visited + tn)
      }
    }

    def addRow(cName: String, cols: Seq[FieldDef], path: List[String], row: Map[String, Any]): Unit = {
      val parent_id = if (path.isEmpty) None else path.tail.find(i => Try(i.toInt).toOption.nonEmpty)
      val id = path.headOption.filter(i => Try(i.toInt).isSuccess).getOrElse("0")
      def addVals(rows: AB[Seq[String]]) = {
        val bind_var_prefix = path.reverse.mkString(":", ".", ".")
        rows += ((id :: parent_id.toList) ++ cols.map { col =>
          val n = col.name
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
          idFields(parent_id.nonEmpty) ++ cols.map(_.name), addVals(AB()), false))
        () // for scala 2.12 compilation
      }
    }

    def traverseData(cName: String, vd: ViewDef, path: List[String], values: Map[String, Any]): Unit = {
      val (complex_fields, primitive_fields) = vd.fields.partition(_.type_.isComplexType)
      if (cName != null && cName.nonEmpty) { //do not make cursor for top level view
        addRow(cName, primitive_fields, path, values)
      }
      complex_fields.foreach { f =>
        val fn = f.name
        val ch_view = viewDef(f.type_.name)
        val ch_cursor = (if (cName == null || cName.isEmpty) "" else cName + "_") + fn
        def hasParentId = path.exists(p => Try(p.toInt).toOption.nonEmpty)
        def emptyCurs = addEmptyCursor(ch_cursor, ch_view, hasParentId, Set())
        values.get(fn).map {
          case lookup: Map[String@unchecked, _] =>
            traverseData(ch_cursor, ch_view, fn :: path, lookup)
          case children: Iterable[_] =>
            val new_path = fn :: path
            val succ = children.zipWithIndex.foldLeft(false) {
              case (_, (m: Map[String@unchecked, _], i)) =>
                traverseData(ch_cursor, ch_view, i.toString :: new_path, m)
                true
              case (r, _) => r || false
            }
            if (!succ) emptyCurs
          case _ => emptyCurs
        }.getOrElse(emptyCurs)
      }
    }

    traverseData("", view, Nil, data)
    cursors.map { case (cName, Cursor(cols, values, isEmpty)) =>
      cName + cols.mkString("(#", ", ", ")") +
        values.map(_.mkString(if (isEmpty) "null[1 = 0]{" else "{", ", ", "}"))
          .mkString(" {", " ++ ", "}")
    }.mkString(", ")
  }

  def cursorsFromBindVars(data: Map[String, Any], cursorPrefix: String): String = {
    type BindVarsCursors = MM[String, AB[MM[String, String]]]
    def addRow(cursors: BindVarsCursors)(cName: String)(id: Int)(parentId: Option[Int]): Unit = {
      def initRow =
        MM[String, String]((BindVarsCursorRowNrColName, id.toString) ::
          parentId.toList.map(BindVarsCursorRowNrRefColName -> _.toString): _*)
      cursors.get(cName)
        .map { cursor => cursor += initRow; () } // return unit explicitly for scala 2.12
        .getOrElse { cursors += (cName -> AB(initRow)); () } // return unit explicitly for scala 2.12
    }
    def addCol(cursors: BindVarsCursors)(cName: String)(path: List[String]): Unit = {
      def col(p: List[String]) =
        Try(p.head.toInt).map(_ => cName).getOrElse(p.head) -> p.reverse.mkString(":", ".", "")
      cursors.get(cName)
        .map(cursor => cursor.last += col(path))
        .getOrElse (cursors += (cName -> AB(MM(col(path)))))
    }
    def traverseData(cursor: String,
                     path: List[String],
                     d: Any,
                     rowFun: String => Int => Option[Int] => Unit,
                     colFun: String => List[String] => Unit): Unit = d match {
      case m: Map[String@unchecked, _] => m.foreach { case (k, v) =>
        traverseData(cursor, k :: path, v, rowFun, colFun)
      }
      case it: Iterable[_] =>
        val cName = path.headOption.map(cursor + "_" + _).getOrElse(cursor)
        def pathLastId(p: List[String]): Option[Int] = p match {
          case Nil => None
          case el :: tail => Try(el.toInt).toOption.orElse(pathLastId(tail))
        }
        val plId = pathLastId(path)
        it.zipWithIndex.foreach { case (v, i) =>
          rowFun(cName)(i)(plId)
          traverseData(cName, i.toString :: path, v, rowFun, colFun)
        }
      case _ => colFun(cursor)(path)
    }
    def tresql(cursors: BindVarsCursors): String = {
      cursors.collect {
        case (cursor_name, rows) if rows.nonEmpty =>
          val cols = rows.head.keys.toSeq.sorted
          val body = rows.map (row => cols.map(row(_)).mkString("{", ", ", "}")).mkString(" ++ ")
          s"$cursor_name(# ${cols.mkString(", ")}) { $body }"
      }.mkString(", ")
    }
    val cursors: BindVarsCursors = MM()
    traverseData(cursorPrefix, Nil, data, addRow(cursors), addCol(cursors))
    tresql(cursors)
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
        f.name -> {
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
}
