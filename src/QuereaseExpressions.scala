package querease

import mojoz.metadata.TableDef

import org.tresql.QueryParser
import org.tresql.QueryParser.Null

import org.tresql.{ CacheBase, SimpleCacheBase }
import org.tresql.parsing.{ QueryParsers, ExpTransformer }

import scala.util.parsing.input.CharSequenceReader
import scala.util.Try
import scala.util.control.NonFatal

object QuereaseExpressions {
  sealed trait MdContext { val name: String }
  case object Field extends MdContext { override val name: String = "field" }
  case object Filter extends MdContext { override val name: String = "filter" }
  case object Resolver extends MdContext { override val name: String = "resolver" }
  sealed trait TransformerContext
  case object RootCtx extends TransformerContext
  case object EqOpCtx extends TransformerContext
  case object OtherOpCtx extends TransformerContext

  private[querease] val IdentifierPatternString = "([\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*\\.)*[\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*"
  private[querease] val IdentifierExtractor = s"($IdentifierPatternString).*".r

  trait Parser extends QueryParsers with ExpTransformer {
    def parse(expr: String): Exp
    def extractVariables(exp: String) =
      traverser(variableExtractor)(Nil)(parse(exp)).reverse
  }
  abstract class DefaultParser extends Parser {
    val cache: Option[CacheBase[Exp]]
    override def ident: MemParser[String] = opt("^") ~ super.ident ^^ {
      case Some(s) ~ ident => s + ident
      case None ~ ident => ident
    }  named "^ident"
    override def parse(expr: String): Exp = {
      cache.flatMap(_.get(expr)).getOrElse {
        try {
          intermediateResults.get.clear
          val e = phrase(exprList)(new CharSequenceReader(expr)) match {
            case Success(r, _) => r
            case x => sys.error(x.toString)
          }
          cache.foreach(_.put(expr, e))
          e
        } finally intermediateResults.get.clear
      }
    }
  }
  object DefaultParser extends DefaultParser {
    override val cache = Some(new SimpleCacheBase[Exp](4096))
  }
}

import QuereaseExpressions._
trait QuereaseExpressions { this: Querease =>
  case class Context(
    viewDef: ViewDef,
    fieldDef: FieldDef,
    fieldName: String,
    baseTableAlias: String,
    pathToAlias: Map[List[String], String],
    mdContext: MdContext,
    transformerContext: TransformerContext,
    addParensToSubquery: Boolean
  )
  val parser: Parser = DefaultParser
  /** Returns error message expression string for resolver
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    */
  protected def resolverErrorMessageExpression(viewName: String,
                                               fieldName: String,
                                               contextName: String,
                                               bindVars: List[String]): String = {
    val varsToStr = bindVars.map(v => s"coalesce($v, 'null')") match {
      case Nil => "coalesce(_, 'null')"
      case v :: Nil => v
      case l => l.mkString("concat_ws(', ', ", ", ", ")")
    }
    s"""'Failed to identify value of "$fieldName" (from $viewName) - ' || $varsToStr"""
  }

  /** Returns resolver expression string - db function call to check resolver result and throw exception
    * when result is not unique or is missing. This is important to avoid silent deletion of data from db.
    * Default implementation is {{{s"checked_resolve(_, array(\$queryString), \$errorMessage)"}}}
    * For postgresql bigint and string resolvers `checked_resolve` db function can be defined as:
    * {{{
    * create or replace function checked_resolve(resolvable text, resolved bigint[], error_message text)
    *   returns bigint as $$
    * begin
    *   if array_length(resolved, 1) > 1 or resolvable is not null and resolved[1] is null then
    *     raise exception sqlstate '235BX' using message = error_message;
    *   else
    *     return resolved[1];
    *   end if;
    * end;
    * $$ language plpgsql immutable;
    *
    * create or replace function checked_resolve(resolvable text, resolved text[], error_message text)
    *   returns text as $$
    * begin
    *   if array_length(resolved, 1) > 1 or resolvable is not null and resolved[1] is null then
    *     raise exception sqlstate '235BX' using message = error_message;
    *   else
    *     return resolved[1];
    *   end if;
    * end;
    * $$ language plpgsql immutable;
    * }}}
    *
    * @param queryString
    * @param errorMessage
    */
  protected def resolverExpression(queryString: String, errorMessage: String, bindVars: List[String]): String = {
    val bvOrPlaceholder = bindVars match {
      case Nil => "_"
      case List(bv) => bv
      case l => l.mkString("coalesce(", ", ", ")")
    }
    s"checked_resolve($bvOrPlaceholder, array($queryString), $errorMessage)"
  }

  /** Returns transformed expression
    *
    * @param expression  expression to be transformed
    * @param viewDef     view this expression is from
    * @param fieldDef    field this expression is from or null
    * @param mdContext   yaml section this expression is from
    * @param baseTableAlias base table alias for filter transformation
    * @param pathToAlias path to alias map for this query
    */
  protected def transformExpression(
      expression: String, viewDef: ViewDef, fieldDef: FieldDef, mdContext: MdContext,
      baseTableAlias: String = null, pathToAlias: Map[List[String], String] = null): String = {
    val fieldName = Option(fieldDef).map(f => Option(f.alias).getOrElse(f.name)).orNull
    val ctx = Context(viewDef, fieldDef, fieldName, baseTableAlias, pathToAlias, mdContext,
      RootCtx, addParensToSubquery = mdContext == Field)
    transformExpression(expression, ctx)
  }

  /** Returns transformed expression
    *
    * @param expression  expression to be transformed
    * @param viewDef     view this expression is from
    * @param fieldName   field name this expression is from or null
    * @param mdContext   yaml section this expression is from
    * @param baseTableAlias base table alias for filter transformation
    * @param pathToAlias path to alias map for this query
    */
  def transformExpression(
      expression: String, viewDef: ViewDef, fieldName: String, mdContext: MdContext,
      baseTableAlias: String, pathToAlias: Map[List[String], String]): String = {
    val fieldDef: FieldDef = null
    val ctx = Context(viewDef, fieldDef, fieldName, baseTableAlias, pathToAlias, mdContext,
      RootCtx, addParensToSubquery = mdContext == Field)
    transformExpression(expression, ctx)
  }

  private def transformExpression(expression: String, ctx: Context): String = {
    expressionTransformer(ctx)(
      parser.parse(expression)
    ).tresql
  }

  /** Returns expression transformer */
  protected def expressionTransformer: parser.TransformerWithState[Context] = parser.transformerWithState { ctx =>
    import parser._
    val viewName = Option(ctx.viewDef).map(_.name).orNull
    def fullContextName =
      s"${ctx.mdContext.name} of $viewName${Option(ctx.fieldName).map("." + _).getOrElse("")}"
    def refViewExpressionString(refViewName: String, refFieldName: String, refFilter: String): String = {
      val view = ctx.viewDef
      // TODO duplicate code with Dto
      def alias = ctx.fieldName
      val refViewDef = Try(viewDef(refViewName)).toOption
        .getOrElse{
          throw new RuntimeException(
            s"View $refViewName referenced from $fullContextName is not found")
        }
      val refFieldDef = refViewDef.fields.find(f => Option(f.alias).getOrElse(f.name) == refFieldName)
        .getOrElse {
          throw new RuntimeException(
            s"Field $refViewName.$refFieldName referenced from $fullContextName is not found")
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
            s"Field $refViewName.$refFieldName referenced from $fullContextName" +
              " can not be joined because table not specified")
        val viewName = view.name
        val tableDef = tableDefOpt.get
        val joinCol = Option(ctx.fieldDef).map(_.saveTo).orNull
        val allRefsTo = allRefs.filter(_.refTable == refViewDef.table)
        val refs = Option(allRefsTo)
          .map { refs =>
            if (resolverIsRefTableAlias) {
              refs.filter(_.defaultRefTableAlias == filterOrResolver)
            } else refs
          }
          .map { refs =>
            if (refs.lengthCompare(1) > 0 && alias != null) {
              Option(refs.filter(_.defaultRefTableAlias == alias))
                .filter(_.nonEmpty)
                .getOrElse(refs)
            } else refs
          }
          .map { refs =>
            if (refs.lengthCompare(1) > 0 && joinCol != null) {
              Option(refs.filter(_.cols.contains(joinCol)))
                .filter(_.nonEmpty)
                .getOrElse(refs)
            } else refs
          }
          .get
        def refErrorMessage(prefix: String) =
          s"$prefix from $table" +
            s" to $refTable (of $refViewName referenced from $fullContextName)" +
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
              traverser(usedNamesExtractor)(Set.empty)(parse(queryColExpr("true")))
            if (usedNames contains tableOrAlias) {
              val fixedTableAlias = unusedName(tableOrAlias, usedNames)
              val colsString = colsRefCols.map(_._1).mkString(", ")
              val qColsString = colsRefCols.map { case (col, refCol) => s"$tableOrAlias.$col" }.mkString(", ")
              s"$fixedTableAlias(# $colsString) {{$qColsString}}" +
                s" $fixedTableAlias [${joinFilter(fixedTableAlias)}] ${queryColExpr(null)}"
            } else {
              queryColExpr(joinFilter(tableOrAlias))
            }
          case _ =>
            throw new RuntimeException(refErrorMessage("Ambiguous refs"))
        }
      } else {
        queryColExpr(refFilter)
      }
    }

    {
      case BinOp("=", lop, rop) =>
        val nctx = ctx.copy(transformerContext = EqOpCtx, addParensToSubquery = true)
        BinOp("=", expressionTransformer(nctx)(lop), expressionTransformer(nctx)(rop))
      case w: With =>
        val nctx = ctx.copy(transformerContext = OtherOpCtx)
        With(w.tables.map(expressionTransformer(nctx)).asInstanceOf[List[WithTable]], expressionTransformer(ctx)(w.query))
      case q @ Query(tables, filter, cols, group, order, offset, limit) =>
        val isViewRef = tables.size == 1 && tables.head.tresql.startsWith("^")
        val isResolver =
          ctx.mdContext == Resolver && ctx.transformerContext == RootCtx ||
          // OR
          ctx.transformerContext == EqOpCtx &&
          (ctx.mdContext == Filter || ctx.mdContext == Resolver) &&
          //resolvableVarOpt.isDefined && // has at least one variable to resolve TODO or underscore or query result
          Option(cols).map(_.cols).filter(_ != null).getOrElse(Nil).size == 1
          // FIXME kā atšķirt rezolveri (ar vienu kolonu) no parasta FieldRefRegexp???
        def withLimitQ(q: Query) =
          (if (q.limit == null) q.copy(limit = Const(2)) else q)
        // TODO transform all fieldrefs of this query
        val resolvedQuery = if (!isViewRef) {
          val nctx = if (ctx.transformerContext == OtherOpCtx) ctx else ctx.copy(transformerContext = OtherOpCtx)
          Query(
            tables = tables.map(t => expressionTransformer(nctx)(t).asInstanceOf[Obj]),
            filter = expressionTransformer(nctx)(filter).asInstanceOf[Filters],
            cols = expressionTransformer(nctx)(cols).asInstanceOf[Cols],
            group = expressionTransformer(nctx)(group).asInstanceOf[Grp],
            order = expressionTransformer(nctx)(order).asInstanceOf[Ord],
            limit = expressionTransformer(nctx)(limit),
            offset = expressionTransformer(nctx)(offset)
          )
        } else {
          val viewRef = tables.head.tresql.substring(1)
          val (refViewName, refFieldName) = cols match {
            case null =>
              viewRef.lastIndexOf(".") match {
                case -1 =>
                  (viewRef, null)
                case ix =>
                  val (refViewName, dotRefFieldName) = viewRef.splitAt(ix)
                  (refViewName, dotRefFieldName.substring(1))
              }

            case _ =>
              (viewRef, null)
          }
          val resolvedQueryString = if (refFieldName != null && (filter == null || filter.filters != null && filter.filters.size == 1)) {
            refViewExpressionString(refViewName, refFieldName, Option(filter).map(_.filters(0).tresql).map(f => f.substring(1, f.length - 1)).orNull)
          } else {
            val refViewDef = viewDefOption(refViewName)
              .getOrElse{
                throw new RuntimeException(
                  s"View $refViewName referenced from $fullContextName is not found")
              }
            // val alias = fieldAliasOrName
            val refViewDefBaseTableAlias = null // FIXME refViewDefBaseTableAlias?
            val refViewDefPathToAlias = null
            val colFields =
              Option(cols)
                .map(_.cols.map(_.tresql))
                .orElse(Option(refFieldName).map(rf => List(rf)))
                .getOrElse(Nil)
                .map(colName => new mojoz.metadata.FieldDef(colName))
                .map(_.copy(table = refViewDef.table, tableAlias = refViewDef.tableAlias))
            def fieldRefExtractor: Traverser[List[String]] = { fieldRefs => {
              case nestedQ: Query =>
                fieldRefs
              case iexpr @ Ident(List(ident)) if ident startsWith "^" =>
                ident :: fieldRefs
              }
            }
            val fieldRefs = traverser(fieldRefExtractor)(Nil)(q.filter).reverse
            val refFields = fieldRefs
              .map(_.substring(1))
              .map { refFieldName =>
                refViewDef.fields.find(f => Option(f.alias).getOrElse(f.name) == refFieldName)
                  .getOrElse {
                    throw new RuntimeException(
                      s"Field $refViewName.$refFieldName referenced from $fullContextName is not found")
                  }
              }
            val transformedFilterString =
              transformFilter(
                q.filter.filters.map(_.tresql).mkString,
                refViewDef, refViewDefBaseTableAlias, refViewDefPathToAlias)
            queryString(refViewDef, colFields, refFields, transformedFilterString)
          }
          parse(resolvedQueryString) match {
            case q: Query => q
            case w: With => w
            case x =>
              sys.error("Unexpected query class: " + Option(x).map(_.getClass.getName).orNull +
                s" in $fullContextName parsed from: $resolvedQueryString")
          }
        }
        if (isResolver) {
          val resolverVars = traverser(variableExtractor)(Nil)(q).reverse
          val resolverVarsTresql = resolverVars.map(_.tresql)
          val resolvableName = Option(ctx.fieldName).orElse(resolverVars.headOption.map(_.variable)).orNull
          val errorMessage = resolverErrorMessageExpression(viewName, resolvableName, ctx.mdContext.name, resolverVarsTresql)
          val resolvedQueryStringWithLimit: String =
            resolvedQuery match {
              case q: Query => withLimitQ(q).tresql
              case w @ With(tables, q: Query) => w.copy(query = withLimitQ(q)).tresql
              case x =>
                sys.error("Unexpected query class: " + Option(x).map(_.getClass.getName).orNull +
                  s" in $fullContextName")
            }
          parse(resolverExpression(resolvedQueryStringWithLimit, errorMessage, resolverVarsTresql))
        } else if (ctx.addParensToSubquery) {
          parse("(" + resolvedQuery.tresql + ")")
        } else {
          resolvedQuery
        }
      case iexpr @ Ident(List(ident)) if ident.startsWith("^") =>
        val name = ident.substring(1)
        lazy val pathToAlias = if (ctx.pathToAlias != null) ctx.pathToAlias else this.fromAndPathToAlias(ctx.viewDef)._2
        Seq(ctx.viewDef).filter(_ != null)
          .flatMap(_.fields)
          .find(f => Option(f.alias).getOrElse(f.name) == name)
          .map(f => parse(Option(f.expression).map(_ => queryColExpression(ctx.viewDef, f, pathToAlias)).getOrElse(
             Option(f.tableAlias).orElse(Option(ctx.baseTableAlias)).map(_ + ".").getOrElse("") + f.name)))
          .getOrElse(iexpr)
      case iexpr @ Ident(identList: List[String]) if identList(0).startsWith("^") =>
        val ident = iexpr.tresql
        val (viewRef, dotRefFieldName) = ident.splitAt(ident.lastIndexOf("."))
        val (refViewName, refFieldName) = (viewRef.substring(1), dotRefFieldName.substring(1))
        val resolvedQueryString = refViewExpressionString(refViewName, refFieldName, refFilter = null)
        if (ctx.addParensToSubquery) {
          parse("(" + resolvedQueryString + ")")
        } else {
          parse(resolvedQueryString)
        }
      case o @ Obj(b: Braces, _, null, _, _) =>
        o.copy(obj = expressionTransformer(ctx.copy(addParensToSubquery = false))(b))
      case o @ Obj(i: Ident, _, null, _, _) =>
        o.copy(obj = expressionTransformer(ctx)(i))
      case x if ctx.transformerContext == RootCtx || ctx.transformerContext == EqOpCtx =>
        expressionTransformer(ctx.copy(transformerContext = OtherOpCtx, addParensToSubquery = x.isInstanceOf[BinOp]))(x)
    }
  }
}
