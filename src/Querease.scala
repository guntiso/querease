package querease

import scala.language.existentials
import scala.language.postfixOps

import org.tresql.Env
import org.tresql.Query
import org.tresql.QueryParser

import mojoz.metadata.DbConventions
import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
import mojoz.metadata._

/*
case class ListFilterType(Field: String, Comparison: String, Value: String) {
  def this(f: String, v: String) = this(f, "=", v)
}
case class ListSortType(
  Field: String,
  Order: String) {
  def this() = this(null, null)
  def this(Field: String) = this(Field, "asc")
}
case class ListRequestType(
  Limit: Int,
  Offset: Int,
  var Filter: Array[ListFilterType],
  Sort: Array[ListSortType]) {
  def this() = this(0, 0, null, null)
  def this(filter: ListFilterType) = this(0, 0, Array(filter), null)
  def this(filters: Array[ListFilterType], sorts: Array[ListSortType]) =
    this(0, 0, filters, sorts)
}
*/

class Querease(quereaseIo: QuereaseIo, builder: QueryStringBuilder) {
  import quereaseIo._
  import builder._

  // extraPropsToSave allows to specify additional columns to be saved that are not present in pojo.
  def save(pojo: AnyRef, extraPropsToSave: Map[String, Any] = null,
    transform: (Map[String, Any]) => Map[String, Any] = m => m,
    forceInsert: Boolean = false): Long =
    saveTo(getViewDef(pojo.getClass).table, pojo, extraPropsToSave, transform, forceInsert)

  def saveTo(tableName: String, pojo: AnyRef, extraPropsToSave: Map[String, Any] = null,
    transform: (Map[String, Any]) => Map[String, Any] = m => m,
    forceInsert: Boolean = false): Long = {
    val pojoPropMap = toSaveableMap(pojo, getViewDef(pojo.getClass))
    val propMap =
      if (extraPropsToSave != null) pojoPropMap ++ extraPropsToSave
      else pojoPropMap
    val transf = if (transform != null) transform else (m: Map[String, Any]) => m
    val (id, isNew) = propMap.get("id").filter(_ != null).map(id =>
      (Some(id.toString.toLong), forceInsert)) getOrElse (None, true)
    if (isNew) {
      val (_, id) = ORT.insert(tableName, transf(propMap))
      id.asInstanceOf[Long]
    } else {
      ORT.update(tableName, transf(propMap))
      id.get
    }
  }

  def countAll[T <: AnyRef](pojoClass: Class[T], params: Map[String, Any],
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map())) = {
    val (tresqlQueryString, paramsMap) =
      queryStringAndParams(getViewDef(pojoClass), params, 0, 0, "", extraFilterAndParams, true)
    Env.log(tresqlQueryString)
    Query.unique[Int](tresqlQueryString, paramsMap)
  }
/*
  def query[T <: AnyRef](pojoClass: Class[T], params: ListRequestType,
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map())): List[T] =
    query(getViewDef(pojoClass), pojoClass, params, extraFilterAndParams)
  def getOrNull[T <: AnyRef](viewClass: Class[T], id: Long,
    extraFilterAndParams: (String, Map[String, Any])): T = {
    val filterDef = Array(new ListFilterType("Id", "=", id.toString))
    val sortDef = Array[ListSortType]()
    val req = ListRequestType(1, 0, filterDef, sortDef)
    query(viewClass, req, extraFilterAndParams).headOption getOrElse null.asInstanceOf[T]
*/
def list[T <: AnyRef](viewClass: Class[T], params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map())): List[T] = {
    val (q, p) = queryStringAndParams(getViewDef(viewClass), params,
        offset, limit, orderBy, extraFilterAndParams)
    list(q, viewClass, p)
  }

  def get[T <: AnyRef](viewClass: Class[T], id: Long,
    extraFilterAndParams: (String, Map[String, Any]) = null): Option[T] = {
    val extraQ = extraFilterAndParams match {
      case null | ("", _) => "id = :id"
      case (x, _) => s"[$x] & [id = :id]"
    }
    val extraP = extraFilterAndParams match {
      case null | (_, null) => Map[String, Any]()
      case (_, m) => m
    }
    val (q, p) = queryStringAndParams(getViewDef(viewClass),
      Map("id" -> id), 0, 2, "", (extraQ, extraP))
    val result = list(q, viewClass, p)
    if (result.size > 1)
      sys.error("Too many rows returned by query for get method for " +
        viewClass.getName)
    result.headOption
  }

  private def list[T <: AnyRef](
    queryStringAndParams: (String, Map[String, Any]),
    instanceClass: Class[T]): List[T] =
    list(queryStringAndParams._1, instanceClass, queryStringAndParams._2)

  def list[T <: AnyRef](query: String, instanceClass: Class[T], params: Map[String, Any]) =
    fromRows(Query(query, params), instanceClass)

  def delete(instance: AnyRef) = {
    val view = getViewDef(instance.getClass)
    val keyMap = getKeyMap(instance, view)
    Query(builder.deleteStatementString(view, keyMap), keyMap)
  }
/*
  def query[T <: AnyRef](view: ViewDef[Type], pojoClass: Class[T], params: ListRequestType,
    extraFilterAndParams: (String, Map[String, Any])) = {
    val (tresqlQueryString, paramsMap) =
      queryStringAndParams(view, params, extraFilterAndParams)
    Env.log(tresqlQueryString)
    list(tresqlQueryString, pojoClass, paramsMap)
  }
 */
}

trait QueryStringBuilder {
  def queryStringAndParams(view: ViewDef[Type], params: Map[String, Any],
    offset: Int = 0, limit: Int = 0, orderBy: String = null,
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map()),
    countAll: Boolean = false): (String, Map[String, Any])
  def deleteStatementString(view: ViewDef[Type], keyMap: Map[String, Any]): String
}

object QueryStringBuilder {
  def default(typeNameToViewDef: (String) => Option[ViewDef[Type]]) =
    new DefaultQueryStringBuilder(typeNameToViewDef)
  class DefaultQueryStringBuilder(typeNameToViewDef: (String) => Option[ViewDef[Type]])
    extends QueryStringBuilder {
    import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
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
  override def queryStringAndParams(view: ViewDef[Type], params: Map[String, Any],
    offset: Int = 0, limit: Int = 0, orderBy: String = null,
    extraFilterAndParams: (String, Map[String, Any]) = (null, Map()),
    countAll: Boolean = false): (String, Map[String, Any]) = {

    val from = this.from(view)
    val where = this.where(view, Option(extraFilterAndParams).map(_._1).orNull)
    val cols = this.cols(view, countAll)
    val groupBy = this.groupBy(view)
    val order = this.order(view, orderBy)
    val values = Option(params) getOrElse Map[String, Any]() // TODO convert?

    import language.existentials
    val (q, limitOffsetPars) =
      limitOffset(from + where + cols + groupBy + order, countAll, limit, offset)
    (q, values ++ extraFilterAndParams._2 ++ limitOffsetPars.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap)
  }

    /*
    val paramsFilter =
      Option(params).map(_.Filter).filter(_ != null).map(_.toList) getOrElse Nil
    import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
    val fieldNameToDefMap = view.fields.map(f => xsdName(Option(f.alias) getOrElse f.name) -> f).toMap
    // FIXME extra order by, injection-safe!
    val safeExpr = List("decode(cnt, null, 0, 1)",
      "decode(sign(next_reregistration_date - sysdate), 1, 0, 0, 0, 1)")
      .map(expr => (expr,
        FieldDef[Type]("", "", "", "", false, null, true, true, expr,
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
    def isRu(f: FieldDef[Type]) = preferRu && f.isI18n
    */
    private def isI18n(f: FieldDef[Type]) = f.isI18n
    def queryColTableAlias(view: ViewDef[Type], f: FieldDef[Type]) =
      Option(f.tableAlias) getOrElse
        (if (f.table == view.table) view.tableAlias else f.table)

    private def getChildViewDef(viewDef: ViewDef[Type], fieldDef: FieldDef[Type]) =
      typeNameToViewDef(fieldDef.type_.name).getOrElse(
        sys.error("Child viewDef not found: " + fieldDef.type_.name +
          " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))

    def queryColExpression(view: ViewDef[Type], f: FieldDef[Type]) = {
      val qName = queryColTableAlias(view, f) + "." + f.name
      if (f.expression != null) f.expression
      else if (f.type_ != null && f.type_.isComplexType) {
        val childViewDef = getChildViewDef(view, f)
        val joinToParent = Option(f.joinToParent) getOrElse ""
        val sortDetails = Option(f.orderBy match {
          case null => "Id" // preserve detail ordering
          case "#" => null
          case ord => // FIXME support multicol asc/desc order by
            ord.replace("#", "").replace("(", "").replace(")", "").trim
        }).map(DbConventions.dbNameToXsdName).orNull
        /*
        lazy val sortDetailsDbName = DbConventions.xsdNameToDbName(sortDetails)
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
        val (tresqlQueryString, _) =
          queryStringAndParams(childViewDef, null, 0, 0, sortDetails)
        "|" + joinToParent + tresqlQueryString
      } // TODO? else if (isI18n(f)) getI18nColumnExpression(qName)
      else qName
    }

    def queryColAlias(f: FieldDef[Type]) =
      Option(f.alias) getOrElse {
        if (f.isExpression && f.expression != null || isI18n(f)) f.name
        else if (f.type_ != null && f.type_.isComplexType && f.isCollection) f.name // FIXME toPlural(f.name)
        else null
      }

    def queryColName(view: ViewDef[Type], f: FieldDef[Type]) =
      Option(f.alias).getOrElse(
        if (isI18n(f)) f.name else queryColTableAlias(view, f) + "." + f.name)

    def cols(view: ViewDef[Type], countAll: Boolean) =
      if (countAll) " {count(*)}"
      else view.fields
        .filter(f => !f.isExpression || f.expression != null)
        .filter(f => !f.isCollection ||
          (f.type_.isComplexType && !countAll && !f.isExpression))
        .map(f => queryColExpression(view, f)
          + Option(queryColAlias(f)).map(" " + _).getOrElse(""))
        .mkString(" {", ", ", "}")

    private def ast(queryString: String) =
      QueryParser.parseExp(queryString).asInstanceOf[QueryParser.Query]
    // TODO remove this method
    private def fromAndWhere(queryString: String) = ast(queryString)
      .copy(cols = null, group = null, order = null, offset = null, limit = null)
      .tresql
    def groupBy(view: ViewDef[Type]) = Option(view.groupBy)
      .filter(_ != "").map(g => s"($g)") getOrElse ""
    /*
                = Option(view.joins).map(ast).map(_.group)
      .filter(_ != null).map(_.tresql) getOrElse ""
    */
    //DELEME when next todo done
    def from(view: ViewDef[Type]) = if (view.joins != null) fromAndWhere(view.joins) else {
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table)
      if (tables.size > 1) {
        tables -= view.table
        // ? is outer join
        tables.map(view.tableAlias + "/" + _ + "?")
          .mkString(view.table + " ", "; ", "")
      } else view.table + " " + view.tableAlias
    }
    /* TODO merge joins, outer join intelligently (according to metadata)
    val from = {
      val jtables = Option(view.joins).map(JoinsParser(view.table, _).map(_.table).toSet) getOrElse Set()
      val tables = view.fields.foldLeft(scala.collection.mutable.Set[String]())(_ += _.table) -- jtables
      val autoBase = if (!jtables.contains(view.table)) view.table + " " + B else null 
      val autoJoins =
        if (tables.size > 1) {
          tables -= view.table
          // B is base table alias, ? is outer join
          tables.map(B + "/" + _ + "?").mkString(view.table + " ", "; ", "")
        } else 
        else 
      List(view.joins, autoJoins, view.joins).filter(_ != null).mkString("; ")
    }
    val where = (filter.map(f =>
      queryColExpression(fieldNameToDef(f._2.Field)) + " " + comparison(f._2.Comparison) +
        " :" + f._1) ++ Option(extraFilterAndParams._1).filter(_ != ""))
      .mkString("[", " & ", "]") match { case "[]" => "" case a => a }
    */
    def where(view: ViewDef[Type], extraFilter: String) =
      List(view.filter, extraFilter)
        .filter(_ != null).filter(_ != "")
        .mkString("[", " & ", "]") match { case "[]" => "" case a => a }

    def order(view: ViewDef[Type], orderBy: String) =
      Option(orderBy).orElse(Option(view.orderBy))
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
        // TODO ora specific!
        // use limit + offset instead of limit because ora dialect not ready
        (query + "@(? ?)", Array(offset, limit + offset))
    }

    def deleteStatementString(view: ViewDef[Type], keyMap: Map[String, Any]) =
      "-" + view.table +
        keyMap.map(_._1).map(c => s"$c = :$c").mkString("[", " & ", "]")

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
}
