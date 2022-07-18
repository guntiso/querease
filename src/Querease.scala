package org.mojoz.querease

import org.mojoz.metadata.Type
import org.mojoz.metadata.in.{Join, JoinsParser}
import org.mojoz.querease.QuereaseMetadata.{BindVarCursorsCmd, BindVarCursorsCmdRegex, BindVarCursorsForViewCmd, BindVarCursorsForViewCmdRegex}
import org.tresql.parsing.{Arr, Exp, Ident, Null, With, Query => QueryParser_Query}
import org.tresql.{Column, InsertResult, ORT, OrtMetadata, Query, Resources, Result, RowLike, UpdateResult}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try
import scala.util.control.NonFatal

class NotFoundException(msg: String) extends Exception(msg)
class ValidationException(msg: String, val details: List[ValidationResult]) extends Exception(msg)
case class ValidationResult(path: List[Any], messages: List[String])

object SaveMethod extends Enumeration {
  type SaveMethod = Value
  val Save, Insert, Update, Upsert = Value
}
import org.mojoz.querease.SaveMethod._

abstract class Querease extends QueryStringBuilder
  with QuereaseMetadata with QuereaseExpressions with FilterTransformer with BindVarsOps { this: QuereaseIo with QuereaseResolvers =>

  private def regex(pattern: String) = ("^" + pattern + "$").r
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  protected val FieldRefRegexp = regex(s"\\^($ident)\\.($ident)\\s*(\\[(.*)\\])?")

  private def ortDbPrefix(db: String): String       = Option(db).map(db => s"@$db:") getOrElse ""
  private def ortAliasSuffix(alias: String): String = Option(alias).map(" " + _) getOrElse ""
  private def tablesToSaveTo(viewDef: ViewDef) =
    if (viewDef.saveTo == null || viewDef.saveTo.isEmpty)
      Option(viewDef.table).filter(_ != "")
        .map(t => Seq(ortDbPrefix(viewDef.db) + t + ortAliasSuffix(viewDef.tableAlias)))
        .getOrElse(throw new RuntimeException(s"Unable to save - target table name for view ${viewDef.name} is not known"))
    else viewDef.saveTo.map(t => if (t startsWith "@") t else ortDbPrefix(viewDef.db) + t)

  // extraPropsToSave allows to specify additional columns to be saved that are not present in pojo.
  @annotation.nowarn("cat=deprecation") // OK to call deprecated save here - same tables will be extracted again
  def save[B <: DTO](
    pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    forceInsert: Boolean = false,
    filter: String = null,
    params: Map[String, Any] = null)(implicit resources: Resources): Long =
    saveToMultiple(
      tablesToSaveTo(viewDef(ManifestFactory.classType(pojo.getClass))),
      pojo,
      extraPropsToSave,
      forceInsert,
      filter,
      params)

  @deprecated("Parameter 'tableName' is ignored, this method will not work as expected and will be removed", "6.1")
  def saveTo[B <: DTO](
    tableName: String, pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    forceInsert: Boolean = false,
    filter: String = null,
    params: Map[String, Any] = null)(implicit resources: Resources): Long =
    saveToMultiple(
      Seq(tableName),
      pojo,
      extraPropsToSave,
      forceInsert,
      filter,
      params)

  @deprecated("Parameter 'tables' is ignored, this method will not work as expected and will be removed", "6.1")
  def saveToMultiple[B <: DTO](
    tables: Seq[String],
    pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    forceInsert: Boolean = false,
    filter: String = null,
    params: Map[String, Any] = null)(implicit resources: Resources): Long = {
    val pojoPropMap = toMap(pojo)
    val view = viewDef(ManifestFactory.classType(pojo.getClass))
    val method = if (forceInsert) Insert else Save
    save(view, pojoPropMap, extraPropsToSave, method, filter, params)
  }

  def save(
    view:   ViewDef,
    data:   Map[String, Any],
    extraPropsToSave: Map[String, Any],
    method: SaveMethod,
    filter: String,
    params: Map[String, Any],
  )(implicit resources: Resources): Long = {
    validate(view, data, Option(params).getOrElse(Map()))
    val propMap = data ++ (if (extraPropsToSave != null) extraPropsToSave
      else Map()) ++ Option(params).getOrElse(Map())
    val keyFields = viewNameToKeyFields(view.name)
    val idName    = keyFields.find(_.type_.name == "long").map(_.fieldName) getOrElse "id"
    val id        = propMap.get(idName).filter(_ != null)
    val resolvedMethod =
      if  (method == Save)
        if (keyFields.forall(f => propMap.get(f.fieldName).orNull == null)) Insert else Update
      else method
    resolvedMethod match {
      case Insert =>
        insert(view, propMap, filter, extraPropsToSave)
      case Update =>
        update(view, propMap, filter, extraPropsToSave)
        Try(id.get.toString.toLong) getOrElse 0L
      case Upsert =>
        upsert(view, propMap, filter, extraPropsToSave) match {
          case (Insert, insertedId) =>
            insertedId
          case (Update, _)          =>
            Try(id.get.toString.toLong) getOrElse 0L
        }
    }
  }

  private def mergeFilters(filterOpt: Option[String], extraFilter: String): Option[String] = {
    if   (extraFilter == null)  filterOpt
    else if (filterOpt.isEmpty) Option(extraFilter)
    else filterOpt.map { f => Seq(f, extraFilter).map(a => s"($a)").mkString(" & ") }
  }

  private def addExtraPropsToMetadata(metadata: OrtMetadata.View, extraPropsToSave: Map[String, Any]): OrtMetadata.View = {
    if (extraPropsToSave == null || extraPropsToSave.isEmpty) {
      metadata
    } else {
      val mdCols  = metadata.properties.map(_.col).toSet
      val xpCols  = extraPropsToSave.keySet
      val missing = xpCols -- mdCols
      if (missing.isEmpty) {
        metadata
      } else {
        metadata.copy(
          properties = metadata.properties ++ missing.map { col =>
            OrtMetadata.Property(
              col   = col,
              value = OrtMetadata.TresqlValue(
                tresql    = s":$col",
                forInsert = true,
                forUpdate = true,
              ),
            )
          }
        )
      }
    }
  }

  protected lazy val typeNameToScalaTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("scala").orNull)
      .filter(_._2 != null)
      .toMap

  protected def typedValue(row: RowLike, index: Int, type_ : Type) =
    row.typed(index, typeNameToScalaTypeName.get(type_.name).orNull)

  def toCompatibleSeqOfMaps(result: Result[RowLike], view: ViewDef): Seq[Map[String, Any]] =
    result.foldLeft(List[Map[String, Any]]()) { (l, childRow) =>
      toCompatibleMap(childRow, view) :: l
    }.reverse

  def toCompatibleMap(row: RowLike, view: ViewDef): Map[String, Any] = {
    var compatibleMap = viewNameToMapZero(view.name)
    def typed(name: String, index: Int) = view.fieldOpt(name).map { field =>
      if (field.type_.isComplexType) {
        val childView = viewDef(field.type_.name)
        val childResult = row.result(index)
        val childSeq = toCompatibleSeqOfMaps(childResult, childView)
        if (field.isCollection)
          childSeq
        else if (childSeq.lengthCompare(0) == 0)
          null
        else if (childSeq.lengthCompare(1) == 0)
          childSeq.head
        else
          sys.error(s"Incompatible result for field $field of view ${view.name} - expected one row, got more")
      } else {
        typedValue(row, index, field.type_)
      }
    }
    for (i <- 0 until row.columnCount) row.column(i) match {
      case Column(_, name, _) if name != null =>
        typed(name, i).foreach(value => compatibleMap += (name -> value))
      case _ =>
    }
    compatibleMap
  }

  /** Provides missing fields, override to convert types to compatible with your jdbc driver */
  protected def toSaveableMap(map: Map[String, Any], view: ViewDef): Map[String, Any] = {
    (if (view.fields.forall(map contains _.fieldName)) map else viewNameToMapZero(view.name) ++ map) ++
      view.fields
        .filter(_.type_.isComplexType)
        .map(f => f.fieldName -> (map.getOrElse(f.fieldName, null) match {
          case null if f.isCollection => Nil
          case null => null
          case Nil  => Nil
          case seq: Seq[_] => seq map {
            case m: Map[String, Any]@unchecked => toSaveableMap(m, viewDef(f.type_.name))
            case x => x
          }
          case m: Map[String, Any]@unchecked => toSaveableMap(m, viewDef(f.type_.name))
          case x => x
        }))
  }

  protected def insert(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(insert = mergeFilters(f.insert, filter))
          },
        )
      }
    insert(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def insert(
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get)
    insert(view, metadata, data)
  }

  protected def insert(
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val result = ORT.insert(metadata, toSaveableMap(data, view))
    val (insertedRowCount, id) =
      (result.count.get, result.id.orNull)
    if (insertedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not inserted into table(s): ${tables.mkString(",")}")
    }
    else id match {
      case id: Long => id
      case id: Int  => id
      case xx       => 0L
    }
  }

  protected def update[B <: DTO](
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources): Unit = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(update = mergeFilters(f.update, filter))
          },
        )
      }
    update(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def update[B <: DTO](
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources): Unit = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get)
    update(view, metadata, data)
  }

  protected def update[B <: DTO](
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources): Unit = {
    val updatedRowCount = ORT.update(metadata, toSaveableMap(data, view)).count getOrElse -1
    if (updatedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not updated in table(s): ${tables.mkString(",")}")
    }
  }

  protected def upsert(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(
              insert = mergeFilters(f.insert, filter),
              update = mergeFilters(f.update, filter),
            )
          },
        )
      }
    upsert(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def upsert(
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val metadata = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get)
    upsert(view, metadata, data)
  }

  protected def upsert(
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val result = ORT.save(metadata, toSaveableMap(data, view))
    val (method, affectedRowCount, id) = result match {
      case ir: InsertResult => (Insert, result.count.get, result.id.orNull)
      case ur: UpdateResult => (Update, result.count.get, 0L)
    }
    if (affectedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not upserted in table(s): ${tables.mkString(",")}")
    } else id match {
      case id: Long => (method, id)
      case id: Int  => (method, id)
      case xx       => (method, 0L)
    }
  }

  def validationsQueryString(viewDef: ViewDef, env: Map[String, Any]): Option[String] = {
    import QuereaseMetadata.AugmentedQuereaseViewDef
    validationsQueryString(viewDef, env, viewDef.validations)
  }

  def validationsQueryString(viewDef: ViewDef,
                             env: Map[String, Any],
                             validations: Seq[String]): Option[String] = {
    val result =
      if (validations != null && validations.nonEmpty) {
        def tresql(vs: Seq[String]): String = {
          def error(exp: Exp) = {
            val msg =
              s"Validation expression must consist of" +
                s" two or three comma separated expressions. One before last is boolean expression" +
                s" which evaluated to false returns last string expression as error message." +
                s" In the case of three expressions first of them should be cursor definitions which can" +
                s" be used later in boolean and error message expressions." +
                s" Instead got: ${exp.tresql}"
            sys.error(msg)
          }

          var cursorList = List[String]()
          val valTresql =
            "messages(# idx, msg) {" +
              vs.zipWithIndex.map {
                case (v, i) =>
                  val valExp =
                    parser.parseExp(v) match {
                      case Arr(valExps) if valExps.size == 3 =>
                        valExps.head match {
                          case With(cursors, _) =>
                            cursorList = cursors.map(_.tresql).mkString(", ") :: cursorList
                          case x => error(x)
                        }
                        valExps.tail.map(_.tresql).mkString(", ")
                      case Arr(valExps) if valExps.size == 2 => v
                      case x => error(x)
                    }
                  s"{ $i idx, if_not($valExp) msg }"
              }.mkString(" + ") +
              "} messages[msg != null] { msg } #(idx)"
          if (cursorList.isEmpty) valTresql
          else cursorList.reverse.mkString(", ") + ", " + valTresql
        }


        val validations_head = validations.head.trim
        def createEnv(bindVars: List[String]) =
          bindVars.foldLeft(Map[String, Any]()) { (res, bv) =>
            env(bv) match {
              case m: Map[String, _]@unchecked => res ++ m
              case x => res + (bv -> x)
            }
          }
        def bindVarsFromRegex(vars: String) =
          vars.split("\\s+")
            .filter(_.nonEmpty)
            .map(_.substring(1)) // remove colon
            .toList
        if (validations_head.startsWith(BindVarCursorsForViewCmd) &&
          BindVarCursorsForViewCmdRegex.pattern.matcher(validations_head).matches) {
          val m = BindVarCursorsForViewCmdRegex.findAllMatchIn(validations_head).toList.head
          val view_name = m.subgroups.head
          val bind_vars =
            if (m.subgroups.tail.nonEmpty) bindVarsFromRegex(m.subgroups.tail.head) else Nil
          val vd = this.viewDef(if (view_name.startsWith(":"))
            String.valueOf(env(view_name.drop(1))) else view_name)
          val values = if (bind_vars.isEmpty) env else createEnv(bind_vars)
          cursorsFromViewBindVars(values, vd) + ", " + tresql(validations.tail)
        } else if (validations_head.startsWith(BindVarCursorsCmd) &&
          BindVarCursorsCmdRegex.pattern.matcher(validations_head).matches) {
          val m = BindVarCursorsCmdRegex.findAllMatchIn(validations_head).toList.head
          val bind_vars = bindVarsFromRegex(m.subgroups.head)
          val values = if (bind_vars.isEmpty) env else createEnv(bind_vars)
          cursorsFromViewBindVars(values, viewDef) + ", " + tresql(validations.tail)
        } else tresql(validations)
      } else null

    Option(result)
  }
  def validationsQueryStrings(viewDef: ViewDef, env: Map[String, Any]): Seq[String] = {
    val visited: collection.mutable.Set[String] = collection.mutable.Set.empty
    def vqsRecursively(viewDef: ViewDef): Seq[String] = {
      if (visited contains viewDef.name) Nil
      else {
        visited += viewDef.name
        validationsQueryString(viewDef, env).toList ++
          viewDef.fields.flatMap { f =>
            if (f.type_.isComplexType)
              vqsRecursively(this.viewDef(f.type_.name))
            else Nil
          }
      }
    }
    vqsRecursively(viewDef)
  }
  def validationResults[B <: DTO](pojo: B, params: Map[String, Any])(implicit resources: Resources): List[ValidationResult] = {
    val view = viewDef(ManifestFactory.classType(pojo.getClass))
    validationResults(view, toMap(pojo), params)
  }
  def validationResults(view: ViewDef, data: Map[String, Any], params: Map[String, Any])(
    implicit resources: Resources): List[ValidationResult] = {
    def validateView(viewDef: ViewDef, obj: Map[String, Any]): List[String] =
      validationsQueryString(viewDef, obj) match {
        case Some(query) =>
          Query(query, obj).map(_.s("msg")).toList
        case _ => Nil
      }
    def validateViewAndSubviews(path: List[Any], viewDef: ViewDef, obj: Map[String, Any],
                                res: List[ValidationResult]): List[ValidationResult] = {
      val viewRes =
        validateView(viewDef, obj ++ params) match {
          case messages if messages.nonEmpty => List(ValidationResult(path.reverse, messages))
          case _ => Nil
        }
      viewDef.fields
        .collect { case f if f.type_.isComplexType => (f.name, nameToViewDef(f.type_.name)) }
        .foldLeft(viewRes ::: res) { (r, nv) =>
          val (n, vd) = nv
          def maybeAddParent(m: Map[String, Any]) =
            if(!m.contains("__parent")) m + ("__parent" -> obj) else m
          obj.get(n).map {
            case m: Map[String, _]@unchecked =>
              validateViewAndSubviews(n :: path, vd, maybeAddParent(m), r)
            case l: List[Map[String, _]@unchecked] =>
              val p = n :: path
              l.zipWithIndex.foldLeft(r){ (r1, owi) =>
                val (o, i) = owi
                validateViewAndSubviews(i :: p, vd, maybeAddParent(o), r1)
              }
            case _ => r
          }.getOrElse(r)
        }
    }
    validateViewAndSubviews(Nil, view, data, Nil).reverse
  }
  def validate[B <: DTO](pojo: B, params: Map[String, Any])(implicit resources: Resources): Unit = {
    val view = viewDef(ManifestFactory.classType(pojo.getClass))
    validate(view, toMap(pojo), params)
  }
  def validate(view: ViewDef, data: Map[String, Any], params: Map[String, Any])(implicit resources: Resources): Unit = {
    val results = validationResults(view, data, params)
    results.flatMap(_.messages).filterNot(_ == null).filterNot(_ == "") match {
      case messages if messages.nonEmpty => throw new ValidationException(messages.mkString("\n"), results)
      case _ => ()
    }
  }

  def countAll[B <: DTO: Manifest](params: Map[String, Any],
    extraFilter: String = null, extraParams: Map[String, Any] = Map())(
      implicit resources: Resources) = {
      countAll_(viewDef[B], params, extraFilter, extraParams)
  }
  protected def countAll_(viewDef: ViewDef, params: Map[String, Any],
    extraFilter: String = null, extraParams: Map[String, Any] = Map())(
      implicit resources: Resources): Int = {
    val (tresqlQueryString, paramsMap) =
      queryStringAndParams(viewDef, params, 0, 0, "", extraFilter, extraParams, true)
    import org.tresql.CoreTypes._
    Query.unique[Int](tresqlQueryString, paramsMap)
  }

  def list[B <: DTO: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilter: String = null, extraParams: Map[String, Any] = Map())(
        implicit resources: Resources): List[B] = {
    result(params, offset, limit, orderBy, extraFilter, extraParams).toList
  }
  def result[B <: DTO: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilter: String = null, extraParams: Map[String, Any] = Map())(
        implicit resources: Resources): CloseableResult[B] = {
    val (q, p) = queryStringAndParams(viewDef[B], params,
        offset, limit, orderBy, extraFilter, extraParams)
    result(q, p)
  }
  def rowsResult(viewDef: ViewDef, params: Map[String, Any],
      offset: Int, limit: Int, orderBy: String, extraFilter: String)(
        implicit resources: Resources): Result[RowLike] = {
    val (q, p) = queryStringAndParams(viewDef, params,
        offset, limit, orderBy, extraFilter, Map.empty)
    Query(q, p)
  }

  def get[B <: DTO](id: Long)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(id), null, null)
  def get[B <: DTO](id: Long, extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(id), extraFilter, null)
  def get[B <: DTO](id: Long, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(id), null, extraParams)
  def get[B <: DTO](id: Long, extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(id), extraFilter, extraParams)

  def get[B <: DTO](code: String)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(code), null, null)
  def get[B <: DTO](code: String, extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(code), extraFilter, null)
  def get[B <: DTO](code: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(code), null, extraParams)
  def get[B <: DTO](code: String, extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(Seq(code), extraFilter, extraParams)

  def get[B <: DTO](keyValues: Seq[Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(keyValues, null, null)
  def get[B <: DTO](keyValues: Seq[Any], extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(keyValues, extraFilter, null)
  def get[B <: DTO](keyValues: Seq[Any], extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = get(keyValues, null, extraParams)

  // TODO if view contains fields for columns - use field names, otherwise use column names?
  def get[B <: DTO](keyValues: Seq[Any], extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = {
    val view = viewDef[B]
    keyValues match {
      case Nil =>
        get(keyValues, Nil, extraFilter, extraParams)
      case Seq(id: Long) =>
        val keyColName = viewNameToKeyColNameForGetById(view.name)
        if (keyColName == null)
          sys.error(s"Key of long type not found for $mf, can not get")
        get(keyValues, Seq(keyColName), extraFilter, extraParams)
      case Seq(code: String) =>
        val keyColName = viewNameToKeyColNameForGetByCode(view.name)
        if (keyColName == null)
          sys.error(s"Key of string type not found for $mf, can not get")
        get(keyValues, Seq(keyColName), extraFilter, extraParams)
      case values =>
        val key_fields = viewNameToKeyFields(view.name)
        if (key_fields.isEmpty)
          sys.error(s"Key not found for $mf, can not get")
        val keyColNames = key_fields.map(_.name)
        get(keyValues, keyColNames, extraFilter, extraParams)
    }
  }
  def get[B <: DTO](keyValues: Seq[Any], keyColNames: Seq[String], extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources): Option[B] = {
    get(viewDef[B], keyValues, keyColNames, extraFilter, extraParams).map { row =>
      val converted = convertRow[B](row)
      row.close()
      converted
    }
  }

  def get(
    viewDef:     ViewDef,
    keyValues:   Seq[Any],
    keyColNames: Seq[String],
    extraFilter: String,
    extraParams: Map[String, Any],
  )(implicit resources: Resources): Option[RowLike] = {
    import viewDef.name
    val qualifier = baseFieldsQualifier(viewDef)
    val prefix = Option(qualifier).map(_ + ".") getOrElse ""
    if (keyColNames.length != keyValues.length)
      sys.error(
        s"Expecting ${keyColNames.length} value(s) as key for $name but got ${keyValues.length}, can not get")
    val keysAndValues = keyColNames zip keyValues
    val keyFilter = keysAndValues.map {
      case (col, value) => s"${prefix}${col} = :${col}"
    }.mkString(" & ")
    val params = keysAndValues.toMap
    val extraQ = extraFilter match {
      case null | "" =>    keyFilter
      case x => s"[$x] & [$keyFilter]"
    }
    val extraP = extraParams match {
      case null => Map[String, Any]()
      case m => m
    }
    val (q, p) = queryStringAndParams(viewDef,
      params, 0, 2, "", extraQ, extraP)
    try Query(q, p).uniqueOption catch {
      case ex: org.tresql.MissingBindVariableException  => throw ex
      case ex: org.tresql.TresqlException               => throw ex
      case util.control.NonFatal(ex) =>
        throw new RuntimeException(
          s"Failed to get unique optional row for view $name: ${ex.getMessage}", ex)
    }
  }

  def create(
    view:   ViewDef,
    params: Map[String, Any],
  )(implicit resources: Resources): RowLike = {
    import QuereaseMetadata.AugmentedQuereaseFieldDef
    import view.name
    val q =
    if (view.fields.exists(_.initial != null)) {
        view.fields.map { f =>
          val expr =
            if (f.initial != null)
              f.initial
            else if (f.type_ != null && f.type_.isComplexType)
              s"|[false]${view.table}[false]{0}" // XXX FIXME providing child result - expected by current QuereaseIo implementation
            else
              "null"
          expr + " " + f.fieldName
        }.mkString("{", ", ", "}")
    } else "{'__fake__' __fake__}"
    try Query(q, params).uniqueOption.get catch {
      case ex: org.tresql.MissingBindVariableException  => throw ex
      case ex: org.tresql.TresqlException               => throw ex
      case util.control.NonFatal(ex) =>
        throw new RuntimeException(
          s"Failed to execute query to create data for new instance, view $name: ${ex.getMessage}", ex)
    }
  }

  def create[B <: DTO](params: Map[String, Any] = Map.empty)(
      implicit mf: Manifest[B], resources: Resources): B = {
    import QuereaseMetadata.AugmentedQuereaseFieldDef
    val view = this.viewDef
    if (view.fields.exists(_.initial != null)) {
      Option(create(view, params)).map { row =>
        val converted = convertRow[B](row)
        row.close()
        converted
      }.get
    } else {
      mf.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[B]
    }
  }

  def list[B <: DTO: Manifest](query: String, params: Map[String, Any])(
    implicit resources: Resources) =
    result(query, params).toList
  def result[B <: DTO: Manifest](query: String, params: Map[String, Any])(
      implicit resources: Resources): CloseableResult[B] =
    new Iterator[B] with AutoCloseable {
      private val result = Query(query, params)
      override def hasNext = result.hasNext
      override def next() = convertRow[B](result.next())
      override def close = result.close
    }

  def delete[B <: DTO](instance: B, filter: String = null, params: Map[String, Any] = null)(
    implicit resources: Resources): Int = {
    val view = viewDef(ManifestFactory.classType(instance.getClass))
    val propMap = toMap(instance)
    delete(view, propMap, filter, params)
  }

  def delete(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    params: Map[String, Any],
  )(implicit resources: Resources): Int = {
    val mergedFilter = nameToPersistenceMetadata.getOrElse(
      view.name, toPersistenceMetadata(view, nameToViewDef, throwErrors = true).get) match {
        case md if filter == null => md.filters.flatMap(_.delete).orNull
        case md => mergeFilters(md.filters.flatMap(_.delete), filter).orNull
      }
    val key_fields = viewNameToKeyFields(view.name)
    if (key_fields.isEmpty)
      sys.error(s"Key not found for view ${view.name}, can not delete")
    val key        = key_fields.map(f => f.name)
    val keyPropMap = key_fields.map(f => f.name -> data.getOrElse(f.fieldName, null)).toMap
    val delCount   = ORT.delete(
      ortDbPrefix(view.db) + view.table + ortAliasSuffix(view.tableAlias),
      key,
      if (params == null || params.isEmpty) keyPropMap else keyPropMap ++ params,
      mergedFilter,
    ).count.get
    if (delCount == 0)
      throw new NotFoundException(s"Record not deleted in table ${view.table}")
    else delCount
  }
}

trait QueryStringBuilder { this: Querease =>
  /*
  val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  def comparison(comp: String) =
    if (ComparisonOps.contains(comp)) comp
    else sys.error("Comparison operator not supported: " + comp)
  // TODO languagePrefs, i18nExpr? overridable?
  def languagePreferences: List[String] = List("lv", "en", "ru")
  def getI18nColumnExpression(qName: String) = {
    val langs = languagePreferences
    val lSuff = langs.map {
      case "lv" => ""
      case "en" => "_eng"
      case "ru" => "_rus"
    }
    lSuff.tail.foldLeft(qName + lSuff(0))((expr, suff) => "nvl(" + expr + ", " + qName + suff + ")")
  }
  */
  /** All queries and dml-s from viewDef for compilation - to test viewDef. Used by sbt-mojoz plugin */
  def allQueryStrings(viewDef: ViewDef): collection.immutable.Seq[String] = {
    if (viewDef.fields != null && viewDef.fields.nonEmpty &&
         (viewDef.table != null || viewDef.joins != null && viewDef.joins.nonEmpty))
      List(
        queryStringAndParams(viewDef, Map.empty)._1
      )
    else Nil
  } ++ validationsQueryStrings(viewDef, emptyData(viewDef))

  protected def unusedName(name: String, usedNames: collection.Set[String]): String = {
    @tailrec
    def unusedName(index: Int): String =
      // FIXME ensure max identifier length for db is not exceeded!
      if (!(usedNames contains (name + "_" + index))) name + "_" + index
      else unusedName(index + 1)
    if (!usedNames.contains(name)) name else unusedName(2)
  }
  def baseFieldsQualifier(view: ViewDef): String = {
    // TODO do not parse multiple times!
    def parsedJoins =
      Option(view.joins).map(joinsParser(view.db, tableAndAlias(view), _))
        .getOrElse(Nil)
    Option(view.tableAlias)
      .orElse(if (view.joins == null || view.joins == Nil) Some(view.table) else None)
      .getOrElse(parsedJoins
        .filter(_.table == view.table).toList match {
          case Join(a, _, _, _) :: Nil =>
            // qualifier, if base table encountered only once
            Option(a) getOrElse view.table
          case Nil => null // no qualifier
          case baseTableJoinsList =>
            // b - base table alias
            if (baseTableJoinsList.exists(_.alias == "b")) "b" else null
        })
  }
  def queryStringAndParams(view: ViewDef, params: Map[String, Any],
    offset: Int = 0, limit: Int = 0, orderBy: String = null,
    extraFilter: String = null, extraParams: Map[String, Any] = Map(),
    countAll: Boolean = false, includeDbPrefix: Boolean = true,
  ): (String, Map[String, Any]) = {
    val (from, pathToAlias) = this.fromAndPathToAlias(view)
    val where = this.where(view, extraFilter, pathToAlias)
    val groupBy = this.groupBy(view)
    val having = this.having(view)
    val simpleCountAll = countAll && groupBy == "" && having == ""
    val cols = this.cols(view, simpleCountAll, pathToAlias)
    val order = this.order(view, orderBy)
    val values = Option(params) getOrElse Map[String, Any]() // TODO convert?
    val dbPrefix = if (includeDbPrefix) Option(view.db).map(db => s"|$db:").getOrElse("") else ""

    val (q1, limitOffsetPars) =
      limitOffset(dbPrefix + from + where + cols + groupBy + having + order, countAll, limit, offset)
    val q = if (countAll && !simpleCountAll) s"($q1) a {count(*)}" else q1
    // Env log q
    // TODO param name?
    (q, values ++ extraParams ++ limitOffsetPars.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap)
  }
  def queryString(view: ViewDef, fields: Seq[FieldDef], exprFields: Seq[FieldDef], filter: String): String = {
    val (from, pathToAlias) = this.fromAndPathToAlias(view, (fields ++ exprFields).filter(_ != null))
    val where = Option(filter).filter(_ != "")
      .map(filter => if (pathToAlias.size > 1) qualify(view, filter, pathToAlias, ignoreUnknownPaths = true) else filter)
      .map("[" + _ + "]").mkString match { case "" => "" case a => a }
    val groupBy = this.groupBy(view)
    val having = this.having(view)
    val cols = "{" +
      fields.map(field =>
        queryColExpression(view, field, pathToAlias) +
          Option(queryColAlias(field)).map(" " + _).getOrElse("")
      ).mkString(", ") +
    "}"
    from + where + " " + cols + groupBy + having
  }

  /*
  val paramsFilter =
    Option(params).map(_.Filter).filter(_ != null).map(_.toList) getOrElse Nil
  import org.mojoz.metadata.Naming.{ dbNameToXsdName => xsdName }
  val fieldNameToDefMap = view.fields.map(f => xsdName(f.fieldName) -> f).toMap
  // FIXME extra order by, injection-safe!
  val safeExpr = List("decode(cnt, null, 0, 1)",
    "decode(sign(next_reregistration_date - sysdate), 1, 0, 0, 0, 1)")
    .map(expr => (expr,
      FieldDef("", "", "", "", false, null, true, true, expr,
        true, false, null, null, null, null, false, "")))
    .toMap
  def fieldNameToDef(f: String) = fieldNameToDefMap.getOrElse(f,
    safeExpr.get(f) getOrElse
      sys.error("Field " + f + " is not available from view " + xsdName(view.name)))
  def isFilterable(f: ListFilterType): Boolean =
    if (!fieldNameToDef(f.Field).isFilterable) sys.error("Calculated field " + f.Field +
      " is not available for filtering from view " + xsdName(view.name))
    else true
  val filteredParams = Option(params).getOrElse(
    new ListRequestType(0, 0, Array(), Array())).copy(Filter =
      paramsFilter.filter(f => !extraParams.contains(f.Field)).filter(isFilterable).toArray)
  import filteredParams.{ Sort => sort, Offset => offset }
  //LIMIT threshold
  // TODO support overridable maxLimit?
  val limit = math.min(100, filteredParams.Limit)
  //list of tuples (bind variable name -> ListFilterType)
  val filter = filteredParams.Filter.groupBy(_.Field).toList.flatMap(f =>
    if (f._2.size > 1) f._2.zipWithIndex.map(t => (f._1 + t._2) -> t._1) else
      f._2.toList.map(f._1 -> _)).toArray

  val preferRu = languagePreferences(0) == "ru"
  def isRu(f: FieldDef) = preferRu && f.isI18n
  */
  private def isI18n(f: FieldDef) = false // f.isI18n
  def queryColTableAlias(view: ViewDef, f: FieldDef): String =
    Option(f.tableAlias) getOrElse
      (if (f.table == view.table) baseFieldsQualifier(view) else f.table)

  private def getChildViewDef(viewDef: ViewDef, fieldDef: FieldDef) =
    scala.util.Try(this.viewDef(fieldDef.type_.name)).toOption.getOrElse(
      sys.error("Child viewDef not found: " + fieldDef.type_.name +
        " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))

  def qualify(view: ViewDef, expression: String,
    pathToAlias: Map[List[String], String], ignoreUnknownPaths: Boolean = false): String = {
    parser.transformTresql(expression, {
      // do not transform subqueries (skip deeper analysis)
      case q: QueryParser_Query => q
      case ident @ Ident(i) if !i.startsWith("^") =>
        if (i.lengthCompare(1) == 0)
          if (tableMetadata.col(view.table, i.head, view.db).isDefined)
            Ident((baseFieldsQualifier(view) :: i).filter(_ != null))
          else ident // no-arg function or unknown pseudo-col
        else if (ignoreUnknownPaths)
          pathToAlias.get(i dropRight 1)
            .map(a => Ident(a :: (i takeRight 1)))
            .getOrElse(ident)
        else Ident(pathToAlias(i dropRight 1) :: (i takeRight 1))
    })
  }
  def queryColExpression(view: ViewDef, f: FieldDef,
      pathToAlias: Map[List[String], String]): String = {
    val qName = Option(queryColTableAlias(view, f))
      .map(_ + "." + f.name) getOrElse f.name // TODO use pathToAlias!
    if (f.expression != null)
      qualify(
        view,
        transformExpression(
          f.expression, view, f, QuereaseExpressions.Field, baseFieldsQualifier(view), pathToAlias),
        pathToAlias
      )
    else if (f.type_ != null && f.type_.isComplexType) {
      val childViewDef = getChildViewDef(view, f)
      val joinToParent = Option(f.joinToParent).orElse {
        if (view.table != null && childViewDef.table != null) {
          val t1 = Option(view.tableAlias).getOrElse(view.table)
          val t2 = Option(childViewDef.tableAlias).getOrElse(childViewDef.table)
          tableMetadata.tableDef(view).refs
            .find(r => r.defaultRefTableAlias == f.name && r.refTable == childViewDef.table)
            .map { r =>
              r.cols.zip(r.refCols).map {
                case (col, refCol) => s"$t1.$col = $t2.$refCol"
              }.mkString("[", " & ", "]")
            }
        }
        else None
      }.getOrElse("")
      val sortDetails = Option(f.orderBy match {
        case null => childViewDef.orderBy match {
          case null | Nil =>
            val prefix = baseFieldsQualifier(childViewDef) match {
              case null => ""
              case q => q + "."
            }
            // preserve detail ordering
            if (childViewDef.table != null)
              tableMetadata.tableDef(childViewDef).pk
                .map(_.cols.map(prefix + _) mkString ", ").orNull
            else null
          case ord => ord mkString ", "
        }
        case "#" => null
        case ord => // FIXME support multicol asc/desc order by
          ord.replace("#", "").replace("(", "").replace(")", "").trim
      }) /* TODO ? .map(Naming.dbNameToXsdName) */ .orNull
      /*
      lazy val sortDetailsDbName = Naming.xsdNameToDbName(sortDetails)
      val isSortFieldIncluded = sortDetails == null ||
        childViewDef.fields.map(_.fieldName).toSet
        .contains(sortDetailsDbName)
      val extendedChildViewDef = // XXX add missing TODO GET RID OF THIS BS
        if (isSortFieldIncluded) childViewDef
        else {
          val fd = FieldDef(childViewDef.table, null, sortDetailsDbName,
            null, false, null, false, true, null, true, false,
            null, null, null, null, false, null)
          childViewDef.copy(fields = childViewDef.fields ++
            Seq(fd.copy(type_ = columnDef(childViewDef, fd).type_)))
        }
      */
      if (childViewDef.table == null && (childViewDef.joins == null || childViewDef.joins == Nil))
        s"|[false]${view.table}[false]{0}" // XXX FIXME providing child result - expected by current QuereaseIo implementation
      else if (view.name == childViewDef.name)
        s"(|$joinToParent)"
      else {
        val (tresqlQueryString, _) =
          queryStringAndParams(childViewDef, null, 0, 0, sortDetails, includeDbPrefix = false)
        val childDbPrefix = Option(childViewDef.db).map(_ + ":").getOrElse("")
        "|" + childDbPrefix + joinToParent + tresqlQueryString
      }
    } // TODO? else if (isI18n(f)) getI18nColumnExpression(qName)
    else qName
  }

  def queryColAlias(f: FieldDef): String =
    Option(f.alias) getOrElse {
      if (f.isExpression && f.expression != null || isI18n(f)) f.name
      else if (f.type_ != null && f.type_.isComplexType && f.isCollection) f.name // FIXME toPlural(f.name)
      else if (f.type_ != null && f.type_.isComplexType) f.name
      else null
    }

  def queryColName(view: ViewDef, f: FieldDef): String =
    Option(f.alias).getOrElse(
      if (isI18n(f)) f.name else queryColTableAlias(view, f) + "." + f.name)

  def cols(view: ViewDef, countAll: Boolean,
    pathToAlias: Map[List[String], String]): String =
    if (countAll) " {count(*)}"
    else view.fields
      .filter(f => !f.isExpression || f.expression != null)
      .filter(f => !f.isCollection ||
        (f.type_.isComplexType && !countAll && !f.isExpression))
      .map(f => queryColExpression(view, f, pathToAlias)
        + Option(queryColAlias(f)).map(" " + _).getOrElse(""))
      .mkString(" {", ", ", "}")
  def groupBy(view: ViewDef): String = Option(view.groupBy)
    .map(_ mkString ", ")
    .filter(_ != "").map(g => s"($g)") getOrElse ""
  def having(view: ViewDef): String = Option(view.having)
    .map(hl => if (hl.lengthCompare(1) > 0) hl.map(h => s"($h)") else hl)
    .map(_ mkString " & ")
    .filter(_ != "").map(h => s"^($h)") getOrElse ""
  def fromAndPathToAlias(view: ViewDef): (String, Map[List[String], String]) =
    fromAndPathToAlias(view, view.fields)
  private def fromAndPathToAlias(view: ViewDef, view_fields: Seq[FieldDef]): (String, Map[List[String], String]) = {
    try fromAndPathToAliasUnhandled(view, view_fields)
    catch {
      case NonFatal(ex) => throw new RuntimeException("Failed to build query for " + view.name, ex)
    }
  }
  private def fromAndPathToAliasUnhandled(view: ViewDef, view_fields: Seq[FieldDef]): (String, Map[List[String], String]) = {
    // TODO prepare (pre-compile) views, use pre-compiled as source!
    // TODO complain if bad paths (no refs or ambiguous refs) etc.
    def simpleName(name: String) = if (name == null) null else name.lastIndexOf('.') match {
      case -1 => name
      case  i => name.substring(i + 1)
    }
    def tailists[B](l: List[B]): List[List[B]] =
      if (l.isEmpty) Nil else l :: tailists(l.tail)
    val (needsBaseTable, parsedJoins) =
      Option(view.joins)
        .map(joins =>
          Try(joinsParser(view.db, null, joins)).toOption
            .map(joins => (false, joins))
            .getOrElse((true, joinsParser(view.db, tableAndAlias(view), joins))))
        .getOrElse((false, Nil))
    val joinAliasToTables: Map[String, Set[String]] =
      parsedJoins.map(j => Option(j.alias).getOrElse(j.table) -> j.table).toSet
        .filter(_._1 != null)
        .flatMap { case (n, t) => tailists(n.split("\\.").toList).map(_.mkString(".") -> t) }
        .groupBy(_._1)
        .map { kkv => kkv._1 -> kkv._2.map(_._2).toSet }
    val joinedAliases = joinAliasToTables.keySet
    val baseQualifier = baseFieldsQualifier(view)
    val baseTableOrAlias = Option(baseQualifier) getOrElse simpleName(view.table)
    val autoBaseAlias =
      if (baseTableOrAlias == view.table) null else baseTableOrAlias
    val autoBase =
      if (view.tableAlias != null && !needsBaseTable &&
        parsedJoins.exists(_.alias == view.tableAlias)) null
      else if (view.tableAlias == null && !needsBaseTable &&
        parsedJoins.lengthCompare(0) > 0) null // TODO ?
      else List(view.table, autoBaseAlias).filter(_ != null) mkString " "
    val pathToAlias = collection.mutable.Map[List[String], String]()
    pathToAlias ++= joinedAliases.map(a => List(a) -> a).toMap
    val usedNames = collection.mutable.Set[String]()
    usedNames ++= joinedAliases
    val aliasToTable = collection.mutable.Map[String, String]()
    if (baseQualifier != null) {
      pathToAlias += (List(baseQualifier) -> baseQualifier)
      usedNames += baseQualifier
      if (view.table != null)
        // FIXME exclude clashing simple names from different qualified names!
        aliasToTable += (baseQualifier -> view.table)
        aliasToTable += (simpleName(baseQualifier) -> view.table)
    }
    aliasToTable ++= joinAliasToTables.filter(_._2.size == 1).map { case (n, t) => n -> t.head }
    def isExpressionOrPath(f: FieldDef) =
      (f.isExpression || f.expression != null) &&
        Option(f.expression).forall(expr =>
          !FieldRefRegexp.pattern.matcher(expr).matches)
    import parser.Traverser
    val IdentifierExtractor: Traverser[List[List[String]]] = {
      identifiers => {
        case i: Ident => i.ident :: identifiers
      }
    }
    val tablePathsInSimpleFields = view_fields
      .filterNot(isExpressionOrPath)
      .map(f => Option(f.tableAlias) getOrElse {
        if (f.table == view.table) baseQualifier else f.table
      })
      .filterNot(_ == null)
      .map(t => List(t))
      .toSet
    val tablePathsInFieldExpressions = view_fields
      .filter(isExpressionOrPath)
      .map(_.expression)
      .filter(_ != null)
      .filter(_.trim != "")
      .map(parser.parseExp)
      // do not collect identifiers from subqueries (skip deeper analysis)
      .map(parser.transformer({ case q: QueryParser_Query => Null }))
      .flatMap(x => parser.traverser(IdentifierExtractor)(Nil)(x))
      .filter(_.lengthCompare(1) > 0)
      .map(_ dropRight 1)
      .toSet
    val tablePaths = tablePathsInSimpleFields ++ tablePathsInFieldExpressions
    val tablePathsNotJoined = {
      def isTableOrAliasInScope(tableOrAlias: String) =
        joinedAliases.contains(tableOrAlias)                                  ||
        baseTableOrAlias == tableOrAlias                                      ||
        tableMetadata.aliasedRef(view.table, tableOrAlias, view.db).isDefined ||
        tableMetadata.tableDefOption(tableOrAlias, view.db).isDefined
      val qualifiedTablePaths = tablePaths map { parts =>
        (1 to parts.length - 1).find { i =>
          isTableOrAliasInScope(parts.take(i).mkString("."))
        } match {
          case None    => parts
          case Some(1) => parts
          case Some(i) => parts.take(i).mkString(".") :: parts.drop(i)
        }
      }
      val qualifiedTablePathsAndPrePaths =
        qualifiedTablePaths.flatMap(p => tailists(p.reverse).map(_.reverse))
      (qualifiedTablePathsAndPrePaths -- joinedAliases.map(List(_)) - List(baseTableOrAlias))
        .toList.sortBy(p => (p.size, p mkString "."))
    }
    tablePathsNotJoined foreach { path =>
      val name = (path takeRight 1).head
      val alias = unusedName(name, usedNames)
      pathToAlias += path -> alias
      usedNames += alias
    }
    val autoJoins = tablePathsNotJoined.map { path =>
      val (contextTable, contextTableOrAlias) =
        if (path.lengthCompare(1) == 0) (view.table, baseTableOrAlias)
        else {
          val contextTableOrAlias = pathToAlias(path dropRight 1)
          val contextTable = aliasToTable(contextTableOrAlias)
          (contextTable, contextTableOrAlias)
        }
      val tableOrAlias = path.last
      val alias = pathToAlias(path)
      // TODO inner join if all steps in path to root are not nullable
      val shouldOuterJoin = true
      val joinMod = if (shouldOuterJoin) "?" else ""
      tableMetadata.aliasedRef(contextTable, tableOrAlias, view.db) match {
        // FIXME support multi-col refs
        case Some(ref) =>
          aliasToTable += alias -> ref.refTable
          s"$contextTableOrAlias[$contextTableOrAlias.${ref.cols(0)} $alias$joinMod] ${ref.refTable}"
        case None =>
          aliasToTable += alias -> tableOrAlias
          contextTableOrAlias + "/" + tableOrAlias + joinMod +
            (if (tableOrAlias == alias) "" else " " + alias)
      }
    }
    val joinsString =
      TresqlJoinsParser.joinsString(
        autoBase,
        List(Option(view.joins).getOrElse(Nil), autoJoins).flatten
          .filter(_ != null).filter(_ != "")
      )
    (joinsString, pathToAlias.toMap)
  }
  /*
  val where = (filter.map(f =>
    queryColExpression(fieldNameToDef(f._2.Field)) + " " + comparison(f._2.Comparison) +
      " :" + f._1) ++ Option(extraFilter).filter(_ != ""))
    .mkString("[", " & ", "]") match { case "[]" => "" case a => a }
  */
  def where(view: ViewDef, extraFilter: String, pathToAlias: Map[List[String], String] = null): String =
    (Option(view.filter).getOrElse(Nil) ++ Option(extraFilter))
      .filter(_ != null).filter(_ != "")
      .map(transformFilter(_, view, baseFieldsQualifier(view), pathToAlias))
      .map("[" + _ + "]").mkString match { case "" => "" case a => a }

  def order(view: ViewDef, orderBy: String): String =
    Option(orderBy).orElse(Option(view.orderBy).map(_ mkString ", "))
    .filter(_ != "").map(o => s"#($o)") getOrElse ""

  /* FIXME
    if (countAll || sort == null || sort.size == 0) ""
    else sort.map(s => (if (s.Order == "desc" || s.Order == "desc null") "~" else "") +
      (fieldNameToDef(s.Field) match {
        case f if f.isExpression && f.expression != null =>
          f.expression
        case f if isRu(f) =>
          "NLSSORT(" + queryColName(f) + ", 'NLS_SORT = RUSSIAN')"
        case f => queryColName(f)
      }) + (if (Option(s.Order).getOrElse("") endsWith " null") " null" else "")).mkString("#(", ", ", ")")
  */

  def limitOffset(query: String, countAll: Boolean, limit: Int, offset: Int
      ): (String, Array[Int]) = (if (countAll) (0, 0) else (limit, offset)) match {
    case (0, 0) => (query, Array())
    case (limit, 0) =>
      (query + "@(?)", Array(limit))
    case (0, offset) =>
      (query + "@(?,)", Array(offset))
    case (limit, offset) =>
      (query + "@(? ?)", Array(offset, limit))
  }

  def tableAndAlias(view: ViewDef): String =
    Option(view.table)
      .map(_ + Option(view.tableAlias).map(" " + _).getOrElse(""))
      .orNull
  lazy val joinsParser: JoinsParser =
    tresqlJoinsParser
/*
  val values = if (filter == null) Map[String, Any]() else filter.map(f => {
    val v = f._2.Value
    // TODO describe convertion error (field, table, value, ...)
    // TODO extract filter type convertion to filter map for overrides
    f._1 -> (fieldNameToDef(f._2.Field).type_.name match {
      case "string" => v
      case "int" => v.toInt
      case "long" => v.toLong
      case "integer" => BigInt(v)
      case "decimal" => BigDecimal(v)
      /*
       * TODO
      case "date" => Format.xsdDate.parse(v)
      case "dateTime" => Format.xsdDateTime.parse(v)
      */
      case "boolean" => v match {
        case "true" | "TRUE" => "Y"
        case "false" | "FALSE" => "N"
        case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
      }
      case x => sys.error("Filter value type not supported: " + x)
    })
  }).toMap
*/
}

trait OracleQueryStringBuilder extends QueryStringBuilder { this: Querease =>
  override def limitOffset(query: String, countAll: Boolean, limit: Int, offset: Int) =
    if (countAll || limit == 0 || offset == 0)
      super.limitOffset(query, countAll, limit, offset)
    else // ora specific!
      // use limit + offset instead of limit because ora dialect not ready
      (query + "@(? ?)", Array(offset, limit + offset))
}
