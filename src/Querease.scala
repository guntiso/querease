package querease

import scala.annotation.tailrec
import scala.language.existentials
import scala.language.postfixOps
import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal
import scala.reflect.ManifestFactory

import org.tresql.Resources
import org.tresql.DeleteResult
import org.tresql.InsertResult
import org.tresql.ArrayResult
import org.tresql.ORT
import org.tresql.Query
import org.tresql.QueryParser
import org.tresql.QueryParser.Ident
import org.tresql.QueryParser.Null
import org.tresql.QueryParser.Traverser
import org.tresql.QueryParser.{ traverser, parseExp }

import mojoz.metadata.in.Join
import mojoz.metadata.in.JoinsParser
import mojoz.metadata._

class NotFoundException(msg: String) extends Exception(msg)

abstract class Querease extends QueryStringBuilder with QuereaseMetadata with QuereaseExpressions with FilterTransformer { this: QuereaseIo =>

  private def regex(pattern: String) = ("^" + pattern + "$").r
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  protected val FieldRefRegexp = regex(s"\\^($ident)\\.($ident)\\s*(\\[(.*)\\])?")

  private def tablesToSaveTo(viewDef: ViewDef) =
    if (viewDef.saveTo == null || viewDef.saveTo.isEmpty)
      Seq(viewDef.table + Option(viewDef.tableAlias).map(" " + _).getOrElse(""))
    else viewDef.saveTo

  // extraPropsToSave allows to specify additional columns to be saved that are not present in pojo.
  def save[B <: DTO](
    pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    transform: (Map[String, Any]) => Map[String, Any] = m => m,
    forceInsert: Boolean = false,
    filterAndParams: (String, Map[String, Any]) = null)(implicit resources: Resources): Long =
    saveToMultiple(
      tablesToSaveTo(viewDef(ManifestFactory.classType(pojo.getClass))),
      pojo,
      extraPropsToSave,
      transform,
      forceInsert,
      filterAndParams)

  def saveTo[B <: DTO](
    tableName: String, pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    transform: (Map[String, Any]) => Map[String, Any] = m => m,
    forceInsert: Boolean = false,
    filterAndParams: (String, Map[String, Any]) = null)(implicit resources: Resources): Long =
    saveToMultiple(
      Seq(tableName),
      pojo,
      extraPropsToSave,
      transform,
      forceInsert,
      filterAndParams)

  def saveToMultiple[B <: DTO](
    tables: Seq[String],
    pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    transform: (Map[String, Any]) => Map[String, Any] = m => m,
    forceInsert: Boolean = false,
    filterAndParams: (String, Map[String, Any]) = null)(implicit resources: Resources): Long = {
    val pojoPropMap = toSaveableMap(pojo)
    val propMap = pojoPropMap ++ (if (extraPropsToSave != null) extraPropsToSave
      else Map()) ++ (if (filterAndParams != null && filterAndParams._2 != null) filterAndParams._2
      else Map())
    val transf = if (transform != null) transform else (m: Map[String, Any]) => m
    val (id, isNew) = propMap.get("id").filter(_ != null).map(id =>
      (Some(id.toString.toLong), forceInsert)) getOrElse (None, true)
    if (isNew) {
      val result =
        if (tables.lengthCompare(1) == 0)
          ORT.insert(tables.head, transf(propMap),
            Option(filterAndParams).map(_._1) orNull)
        else
          ORT.insertMultiple(transf(propMap), tables: _*)(
            Option(filterAndParams).map(_._1) orNull)
      val (insertedRowCount, id) = result match {
        case x: InsertResult => (x.count.get, x.id.get)
        case a: ArrayResult[_] => //if array result consider last element as insert result
           a.values.last match {
            case x: InsertResult => (x.count.get, x.id.get)
          }
      }
      if (insertedRowCount == 0) throw new NotFoundException(
        s"Record not inserted into table(s): ${tables.mkString(",")}")
      else id.asInstanceOf[Long]
    } else {
      val updatedRowCount = if (tables.lengthCompare(1) == 0)
        ORT.update(tables(0), transf(propMap),
          Option(filterAndParams).map(_._1) orNull)
      else
        ORT.updateMultiple(transf(propMap), tables: _*)(
          Option(filterAndParams).map(_._1) orNull)
      if (updatedRowCount == 0) throw new NotFoundException(
        s"Record not updated in table(s): ${tables.mkString(",")}")
      else id.get
    }
  }

  def countAll[B <: DTO: Manifest](params: Map[String, Any],
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map()))(
      implicit resources: Resources) = {
      countAll_(viewDef[B], params, extraFilterAndParams)
  }
  protected def countAll_(viewDef: ViewDef, params: Map[String, Any],
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map()))(
      implicit resources: Resources): Int = {
    val (tresqlQueryString, paramsMap) =
      queryStringAndParams(viewDef, params, 0, 0, "", extraFilterAndParams, true)
    import org.tresql.CoreTypes._
    Query.unique[Int](tresqlQueryString, paramsMap)
  }

  def list[B <: DTO: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilterAndParams: (String, Map[String, Any]) = (null, Map()))(
        implicit resources: Resources): List[B] = {
    result(params, offset, limit, orderBy, extraFilterAndParams).toList
  }
  def result[B <: DTO: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilterAndParams: (String, Map[String, Any]) = (null, Map()))(
        implicit resources: Resources): CloseableResult[B] = {
    val (q, p) = queryStringAndParams(viewDef[B], params,
        offset, limit, orderBy, extraFilterAndParams)
    result(q, p)
  }

  def get[B <: DTO](id: Long, extraFilterAndParams: (String, Map[String, Any]) = null)(
      implicit mf: Manifest[B], resources: Resources): Option[B] = {
    // TODO do not use id and long, get key from tableDef
    val qualifier = baseFieldsQualifier(viewDef[B])
    val prefix = Option(qualifier).map(_ + ".") getOrElse ""
    val extraQ = extraFilterAndParams match {
      case null | (null, _) | ("", _) => s"${prefix}id = :id"
      case (x, _) => s"[$x] & [${prefix}id = :id]"
    }
    val extraP = extraFilterAndParams match {
      case null | (_, null) => Map[String, Any]()
      case (_, m) => m
    }
    val (q, p) = queryStringAndParams(viewDef[B],
      Map("id" -> id), 0, 2, "", (extraQ, extraP))
    val result = list(q, p)
    if (result.lengthCompare(1) > 0)
      sys.error("Too many rows returned by query for get method for " + mf)
    result.headOption
  }

  def create[B <: DTO](params: Map[String, Any] = Map.empty)(implicit mf: Manifest[B]): B = {
    val viewDef = this.viewDef
    viewDef.fields.collect { case f if f.default != null =>
      f.default + " " + Option(queryColAlias(f)).getOrElse(queryColName(viewDef, f))
    } match {
      case x if x.isEmpty => mf.runtimeClass.newInstance.asInstanceOf[B]
      case defaultVals =>
        val query = defaultVals.mkString("{", ", ", "}")
        convertRow(Query(query, params).head)
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
      override def next = convertRow[B](result.next)
      override def close = result.close
    }

  def delete[B <: DTO](instance: B, filterAndParams: (String, Map[String, Any]) = null)(
    implicit resources: Resources) = {
    val view = viewDef(ManifestFactory.classType(instance.getClass))
    val keyMap = this.keyMap(instance)
    val (filter, params) = Option(filterAndParams).getOrElse((null, null))
    val result = ORT.delete(
      view.table + Option(view.tableAlias).map(" " + _).getOrElse(""),
      keyMap.head._2,
      filter,
      params) match { case r: DeleteResult => r.count.get }
    if (result == 0)
      throw new NotFoundException(s"Record not deleted in table ${view.table}")
    else result
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
  private def unusedName(name: String, usedNames: collection.Set[String]): String = {
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
      Option(view.joins).map(joinsParser(tableAndAlias(view), _))
        .getOrElse(Nil)
    Option(view.tableAlias)
      .orElse(if (view.joins == null || view.joins == Nil) Some(view.table) else None)
      .getOrElse(parsedJoins
        .filter(_.table == view.table).toList match {
          case Join(a, _, _) :: Nil =>
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
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map()),
    countAll: Boolean = false): (String, Map[String, Any]) = {

    val (from, pathToAlias) = this.fromAndPathToAlias(view)
    val where = this.where(view, Option(extraFilterAndParams).map(_._1).orNull)
    val groupBy = this.groupBy(view)
    val having = this.having(view)
    val simpleCountAll = countAll && groupBy == "" && having == ""
    val cols = this.cols(view, simpleCountAll, pathToAlias)
    val order = this.order(view, orderBy)
    val values = Option(params) getOrElse Map[String, Any]() // TODO convert?

    import language.existentials
    val (q1, limitOffsetPars) =
      limitOffset(from + where + cols + groupBy + having + order, countAll, limit, offset)
    val q = if (countAll && !simpleCountAll) s"($q1) a {count(*)}" else q1
    // Env log q
    // TODO param name?
    (q, values ++ extraFilterAndParams._2 ++ limitOffsetPars.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap)
  }
  def queryString(view: ViewDef, fields: Seq[FieldDef], exprFields: Seq[FieldDef], filter: String): String = {
    val (from, pathToAlias) = this.fromAndPathToAlias(view, (fields ++ exprFields).filter(_ != null))
    val where = Option(filter).filter(_ != "")
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
  import mojoz.metadata.Naming.{ dbNameToXsdName => xsdName }
  val fieldNameToDefMap = view.fields.map(f => xsdName(Option(f.alias) getOrElse f.name) -> f).toMap
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
      paramsFilter.filter(f => !extraFilterAndParams._2.contains(f.Field)).filter(isFilterable).toArray)
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
  def queryColTableAlias(view: ViewDef, f: FieldDef) =
    Option(f.tableAlias) getOrElse
      (if (f.table == view.table) baseFieldsQualifier(view) else f.table)

  private def getChildViewDef(viewDef: ViewDef, fieldDef: FieldDef) =
    scala.util.Try(this.viewDef(fieldDef.type_.name)).toOption.getOrElse(
      sys.error("Child viewDef not found: " + fieldDef.type_.name +
        " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))

  def qualify(view: ViewDef, expression: String,
    pathToAlias: Map[List[String], String]) = {
    QueryParser.transformTresql(expression, {
      // do not transform subqueries (skip deeper analysis)
      case q: QueryParser.Query => q
      case Ident(i) =>
        if (i.lengthCompare(1) == 0)
          if (tableMetadata.col(view.table, i.head).isDefined)
            Ident((baseFieldsQualifier(view) :: i).filter(_ != null))
          else Ident(i) // no-arg function or unknown pseudo-col
        else Ident(pathToAlias(i dropRight 1) :: (i takeRight 1))
    })
  }
  def queryColExpression(view: ViewDef, f: FieldDef,
      pathToAlias: Map[List[String], String]) = {
    val qName = Option(queryColTableAlias(view, f))
      .map(_ + "." + f.name) getOrElse f.name // TODO use pathToAlias!
    if (f.expression != null)
      if (FieldRefRegexp.pattern.matcher(f.expression).matches) {
        f.expression match {
          case FieldRefRegexp(refViewName, refFieldName, _, refFilter) =>
            // TODO duplicate code with Dto
            def alias = Option(f.alias).getOrElse(f.name)
            val refViewDef = Try(viewDef(refViewName)).toOption
              .getOrElse{
                throw new RuntimeException(
                  s"View $refViewName referenced from ${view.name}.$alias is not found")
              }
            val refFieldDef = refViewDef.fields.find(f => Option(f.alias).getOrElse(f.name) == refFieldName)
              .getOrElse {
                throw new RuntimeException(
                  s"Field $refViewName.$refFieldName referenced from ${view.name}.$alias is not found")
              }
            def queryColExpr(filter: String) =
              queryString(refViewDef, Seq(refFieldDef), Nil, filter)
            val table = view.table
            val tableOrAlias = Option(view.tableAlias).getOrElse(view.table)
            val refTable = refViewDef.table
            val refTableOrAlias = Option(refViewDef.tableAlias).getOrElse(refViewDef.table)
            val tableDefOpt = Option(table).map(tableMetadata.tableDef)
            val key = tableDefOpt
              .map(_.pk).filter(_ != null).filter(_.isDefined).flatten
              .getOrElse(tableDefOpt
                .map(_.uk).filter(_ != null).map(_.filter(_ != null)).filter(_.nonEmpty).map(_.head)
                .orNull)
            val allRefs = tableDefOpt.map { tableDef =>
              if (table == refTable && key != null)
                tableDef.refs :+
                  TableDef.Ref(
                    tableDef.name, key.cols,
                    tableDef.name, key.cols,
                    null, "this", null, null)
               else
                 tableDef.refs
            } getOrElse Nil
            val filterOrResolver = Option(refFilter).map(_.trim).filter(_ != "").orNull
            val resolverIsRefTableAlias =
              filterOrResolver != null &&
                (filterOrResolver == "this" || allRefs.exists(_.defaultRefTableAlias == filterOrResolver))
            if (filterOrResolver == null || resolverIsRefTableAlias) {
              if (table == null || refTable == null)
                throw new RuntimeException(
                  s"Field $refViewName.$refFieldName referenced from ${view.name}.$alias" +
                    " can not be joined because table not specified")
              val viewName = view.name
              val tableDef = tableDefOpt.get
              val joinCol = f.saveTo
              val allRefsTo = allRefs.filter(_.refTable == refViewDef.table)
              val refs = Option(allRefsTo)
                .map { refs =>
                  if (resolverIsRefTableAlias) {
                    refs.filter(_.defaultRefTableAlias == filterOrResolver)
                  } else refs
                }
                .map { refs =>
                  if (refs.lengthCompare(1) > 0) {
                    Option(refs.filter(_.defaultRefTableAlias == alias))
                      .filter(_.nonEmpty)
                      .getOrElse(refs)
                  } else refs
                }
                .map { refs =>
                  if (refs.lengthCompare(1) > 0) {
                    Option(refs.filter(_.cols.contains(joinCol)))
                      .filter(_.nonEmpty)
                      .getOrElse(refs)
                  } else refs
                }
                .get
              def refErrorMessage(prefix: String) =
                s"$prefix from $table" +
                  s" to $refTable (of $refViewName referenced from $viewName.$alias)" +
                  ", please provide resolver or filter explicitly"
              refs.size match {
                case 0 =>
                  throw new RuntimeException(refErrorMessage("No ref found"))
                case 1 =>
                  val colsRefCols = refs.head.cols.zip(refs.head.refCols)
                  def joinFilter(tableOrAlias: String) = colsRefCols.map {
                    case (col, refCol) => s"$refTableOrAlias.$refCol = $tableOrAlias.$col"
                  }.mkString(" & ")
                  def usedNamesExtractor: Traverser[Set[String]] = {
                    names => { case i: Ident => names + i.ident.head }
                  }
                  val usedNames =
                    traverser(usedNamesExtractor)(Set.empty)(parseExp(queryColExpr("true")))
                  if (usedNames contains tableOrAlias) {
                    val fixedTableAlias = unusedName(tableOrAlias, usedNames)
                    val colsString = colsRefCols.map(_._1).mkString(", ")
                    val qColsString = colsRefCols.map { case (col, refCol) => s"$tableOrAlias.$col" }.mkString(", ")
                    s"($fixedTableAlias(# $colsString) {{$qColsString}}" +
                      s" $fixedTableAlias [${joinFilter(fixedTableAlias)}] ${queryColExpr(null)})"
                  } else {
                    "(" + queryColExpr(joinFilter(tableOrAlias)) + ")"
                  }
                case _ =>
                  throw new RuntimeException(refErrorMessage("Ambiguous refs"))
              }
            } else {
              "(" + queryColExpr(refFilter) + ")"
            }
        }
      } else {
       qualify(view, f.expression, pathToAlias)
      }
    else if (f.type_ != null && f.type_.isComplexType) {
      val childViewDef = getChildViewDef(view, f)
      val joinToParent = Option(f.joinToParent) getOrElse ""
      val sortDetails = Option(f.orderBy match {
        case null => childViewDef.orderBy match {
          case null | Nil =>
            val prefix = baseFieldsQualifier(childViewDef) match {
              case null => ""
              case q => q + "."
            }
            // preserve detail ordering
            tableMetadata.tableDef(childViewDef).pk
              .map(_.cols.map(prefix + _) mkString ", ").orNull
          case ord => ord mkString ", "
        }
        case "#" => null
        case ord => // FIXME support multicol asc/desc order by
          ord.replace("#", "").replace("(", "").replace(")", "").trim
      }) /* TODO ? .map(Naming.dbNameToXsdName) */ .orNull
      /*
      lazy val sortDetailsDbName = Naming.xsdNameToDbName(sortDetails)
      val isSortFieldIncluded = sortDetails == null ||
        childViewDef.fields.map(f => Option(f.alias) getOrElse f.name).toSet
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
      if (view.name == childViewDef.name)
        s"(|$joinToParent)"
      else {
        val (tresqlQueryString, _) =
          queryStringAndParams(childViewDef, null, 0, 0, sortDetails)
        "|" + joinToParent + tresqlQueryString
      }
    } // TODO? else if (isI18n(f)) getI18nColumnExpression(qName)
    else qName
  }

  def queryColAlias(f: FieldDef) =
    Option(f.alias) getOrElse {
      if (f.isExpression && f.expression != null || isI18n(f)) f.name
      else if (f.type_ != null && f.type_.isComplexType && f.isCollection) f.name // FIXME toPlural(f.name)
      else if (f.type_ != null && f.type_.isComplexType) f.name
      else null
    }

  def queryColName(view: ViewDef, f: FieldDef) =
    Option(f.alias).getOrElse(
      if (isI18n(f)) f.name else queryColTableAlias(view, f) + "." + f.name)

  def cols(view: ViewDef, countAll: Boolean,
    pathToAlias: Map[List[String], String]) =
    if (countAll) " {count(*)}"
    else view.fields
      .filter(f => !f.isExpression || f.expression != null)
      .filter(f => !f.isCollection ||
        (f.type_.isComplexType && !countAll && !f.isExpression))
      .map(f => queryColExpression(view, f, pathToAlias)
        + Option(queryColAlias(f)).map(" " + _).getOrElse(""))
      .mkString(" {", ", ", "}")
  def groupBy(view: ViewDef) = Option(view.groupBy)
    .map(_ mkString ", ")
    .filter(_ != "").map(g => s"($g)") getOrElse ""
  def having(view: ViewDef) = Option(view.having)
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
    def isExpressionOrPath(f: FieldDef) =
      (f.isExpression || f.expression != null) &&
        Option(f.expression).forall(expr =>
          !FieldRefRegexp.pattern.matcher(expr).matches)
    val (needsBaseTable, parsedJoins) =
      Option(view.joins)
        .map(joins =>
          Try(joinsParser(null, joins)).toOption
            .map(joins => (false, joins))
            .getOrElse((true, joinsParser(tableAndAlias(view), joins))))
        .getOrElse((false, Nil))
    val joinAliasToTable =
      parsedJoins.map(j => (Option(j.alias) getOrElse j.table, j.table)).toMap
    val joined = joinAliasToTable.keySet
    val baseQualifier = baseFieldsQualifier(view)
    val usedInFields = view_fields
      .filterNot(isExpressionOrPath)
      .map(f => Option(f.tableAlias) getOrElse {
        if (f.table == view.table) baseQualifier else f.table
      })
      .filterNot(_ == null)
      .map(t => List(t))
      .toSet
    val IdentifierExtractor: QueryParser.Traverser[List[List[String]]] = {
      identifiers => {
        case i: Ident => i.ident :: identifiers
      }
    }
    val usedInExpr = view_fields
      .filter(isExpressionOrPath)
      .map(_.expression)
      .filter(_ != null)
      .filter(_.trim != "")
      .map(QueryParser.parseExp)
      // do not collect identifiers from subqueries (skip deeper analysis)
      .map(QueryParser.transformer({ case q: QueryParser.Query => Null }))
      .flatMap(x => QueryParser.traverser(IdentifierExtractor)(Nil)(x))
      .filter(_.lengthCompare(1) > 0)
      .map(_ dropRight 1)
      .toSet
    val used = usedInFields ++ usedInExpr
    def tailists[B](l: List[B]): List[List[B]] =
      if (l.isEmpty) Nil else l :: tailists(l.tail)
    val usedAndPrepaths =
      used.flatMap(u => tailists(u.reverse).reverse.map(_.reverse))
    val baseTableOrAlias = Option(baseQualifier) getOrElse view.table
    val missing =
      (usedAndPrepaths -- joined.map(List(_)) - List(baseTableOrAlias))
        .toList.sortBy(p => (p.size, p mkString "."))
    val autoBaseAlias =
      if (baseTableOrAlias == view.table) null else baseTableOrAlias
    val autoBase =
      if (view.tableAlias != null && !needsBaseTable &&
        parsedJoins.exists(_.alias == view.tableAlias)) null
      else if (view.tableAlias == null && !needsBaseTable &&
        parsedJoins.lengthCompare(0) > 0) null // TODO ?
      else List(view.table, autoBaseAlias).filter(_ != null) mkString " "
    val pathToAlias = mutable.Map[List[String], String]()
    pathToAlias ++= joined.map(j => List(j) -> j).toMap
    val usedNames = mutable.Set[String]()
    usedNames ++= joined
    val aliasToTable = mutable.Map[String, String]()
    if (baseQualifier != null) {
      pathToAlias += (List(baseQualifier) -> baseQualifier)
      usedNames += baseQualifier
      if (view.table != null)
        aliasToTable += (baseQualifier -> view.table)
    }
    aliasToTable ++= joinAliasToTable
    missing foreach { path =>
      val name = (path takeRight 1).head
      val alias = unusedName(name, usedNames)
      pathToAlias += path -> alias
      usedNames += alias
    }
    val autoJoins = missing.map { path =>
      val (contextTable, contextTableOrAlias) =
        if (path.lengthCompare(1) == 0) (view.table, baseTableOrAlias)
        else {
          val contextTableOrAlias = pathToAlias(path dropRight 1)
          val contextTable = aliasToTable(contextTableOrAlias)
          (contextTable, contextTableOrAlias)
        }
      val tableOrAlias = (path takeRight 1).head
      val alias = pathToAlias(path)
      // TODO inner join if all steps in path to root are not nullable
      val shouldOuterJoin = true
      val joinMod = if (shouldOuterJoin) "?" else ""
      tableMetadata.ref(contextTable, tableOrAlias) match {
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
    (List(List(autoBase), view.joins, autoJoins).flatten
      .filter(_ != null).filter(_ != "") mkString "; ",
      pathToAlias.toMap)
  }
  /*
  val where = (filter.map(f =>
    queryColExpression(fieldNameToDef(f._2.Field)) + " " + comparison(f._2.Comparison) +
      " :" + f._1) ++ Option(extraFilterAndParams._1).filter(_ != ""))
    .mkString("[", " & ", "]") match { case "[]" => "" case a => a }
  */
  def where(view: ViewDef, extraFilter: String) =
    (Option(view.filter).getOrElse(Nil) ++ Option(extraFilter))
      .filter(_ != null).filter(_ != "")
      .map(transformFilter(_, view, baseFieldsQualifier(view)))
      .map("[" + _ + "]").mkString match { case "" => "" case a => a }

  def order(view: ViewDef, orderBy: String) =
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
      ) = (if (countAll) (0, 0) else (limit, offset)) match {
    case (0, 0) => (query, Array())
    case (limit, 0) =>
      (query + "@(?)", Array(limit))
    case (0, offset) =>
      (query + "@(?,)", Array(offset))
    case (limit, offset) =>
      (query + "@(? ?)", Array(offset, limit))
  }

  def tableAndAlias(view: ViewDef) =
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
