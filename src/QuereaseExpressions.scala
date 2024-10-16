package org.mojoz.querease

import org.mojoz.metadata.ViewDef
import org.mojoz.metadata.FieldDef
import org.mojoz.metadata.TableMetadata
import org.mojoz.metadata.Type
import org.tresql.{Cache, SimpleCache}
import org.tresql.parsing.{ExpTransformer, QueryParsers}
import org.tresql.ast.{Exp, Ident, Ord, Variable}

import scala.util.parsing.input.CharSequenceReader
import scala.util.Try
import scala.collection.immutable.{Seq, Set}

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
  private val IdentR = s"^$IdentifierPatternString$$".r
  private val SimpleIdentR = ("^[_\\p{IsLatin}][_\\p{IsLatin}0-9]*$").r

  trait Parser extends QueryParsers with ExpTransformer {
    val Placeholder = Variable(null, null, false)
    val cache: Option[Cache]
    def extractVariables(exp: String) =
      traverser(variableExtractor)(Nil)(parseExp(exp)).reverse
    def extractPlaceholdersAndVariables(exp: String) =
      traverser(placeholderAndVariableExtractor)(Nil)(parseExp(exp)).reverse
    def transformTresql(tresql: String, transformer: Transformer): String =
      this.transformer(transformer)(parseExp(tresql)).tresql
    /** Extract placeholders and variables in reverse order. Variable names '?' are replaced with index starting with 1 */
    def placeholderAndVariableExtractor: Traverser[List[Variable]] = {
      var bindIdx = 0
      vars => {
        case v @ Variable("?", _, _) =>
          bindIdx += 1
          (v copy bindIdx.toString) :: vars
        case v: Variable => v :: vars
        case Ident(List("_")) => Placeholder :: vars
      }
    }
    def parseWithParser[T](p: Parser.this.Parser[T])(expr:String): T = {
      phrase(p)(new CharSequenceReader(expr)) match {
        case Success(r, _) => r
        case x => sys.error(x.toString)
      }
    }
    def colAndOrd: MemParser[(Exp, Ord)] = opt(expr) ~ opt(order) ^^ {
      case Some(q: org.tresql.ast.Query) ~ None if q.order != null => (q.copy(order = null), q.order)
      case e ~ o => (e.orNull, o.orNull)
    }
  }
  class DefaultParser(val cache: Option[Cache]) extends Parser {
    override def ident: MemParser[String] = (opt("^") ~ super.ident | "^") ^^ {
      case Some(s: String) ~ (ident: String) => s + ident
      case None ~ (ident: String) => ident
      case ident: String => ident
    }  named "^ident"
    override def parseExp(expr: String): Exp = {
      cache.flatMap(_.get(expr)).getOrElse {
        val e = phrase(exprList)(new CharSequenceReader(expr)) match {
          case Success(r, _) => r
          case x => sys.error(x.toString)
        }
        cache.foreach(_.put(expr, e))
        e
      }
    }
  }
}

import QuereaseExpressions._
trait QuereaseExpressions {
  this: QuereaseMetadata with QueryStringBuilder with FilterTransformer
  with QuereaseResolvers with QueryStringBuilder =>
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

  protected def createParserCache: Option[Cache] = Some(new SimpleCache(parserCacheSize, "Querease parser cache"))
  val parser: Parser = new DefaultParser(createParserCache)

  /** Returns missing var expression for inclusion in resolver error message expression */
  protected def resolvablesMessageMissingVarExpression(bindVar: String) = "'[missing]'"

  /** Returns tresql cast expression (some databases enforce more casts than other) */
  protected def resolvableCastToText(typeOpt: Option[Type]) =
    if (typeOpt.isDefined && typeOpt.get.name == "string") "" else "::text"

  /** Returns resolvables expression for inclusion in resolver error message expression
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param bindVars    placeholder or variable expression strings
    */
  protected def resolvablesMessageExpression(
      viewName: String, fieldName: String, contextName: String, bindVars: List[(String, Option[Type])]): String = {
    def expr(v: String, typeOpt: Option[Type]) = {
      val cast = resolvableCastToText(typeOpt)
      if (v.startsWith(":") && v.endsWith("?"))
         s"if_defined_or_else($v, coalesce($v$cast, 'null'), ${resolvablesMessageMissingVarExpression(v)})"
      else s"coalesce($v$cast, 'null')"
    }
    val placeholder = bindVars.find(_._1 == "_")
    if (placeholder.isDefined)
      expr("_", placeholder.get._2)
    else bindVars.map(v => expr(v._1, v._2)) match {
      case Nil => expr("_", None)
      case List(v) => v
      case l => l.mkString("concat_ws(', ', ", ", ", ")")
    }
  }

  /** Returns error message expression for resolver
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param resolvablesMessageExpression   expression for displaying values
    */
  protected def resolverErrorMessageExpression(
      viewName: String, fieldName: String, contextName: String, resolvablesMessageExpression: String): String =
    s"""'Failed to identify value of "$fieldName" (from $viewName) - ' || $resolvablesMessageExpression"""

  /** Returns error message expression for resolver
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param bindVars    placeholder or variable expression strings
    */
  protected def resolverErrorMessageExpression(
      viewName: String, fieldName: String, contextName: String, bindVars: List[(String, Option[Type])]): String =
    resolverErrorMessageExpression(viewName, fieldName, contextName,
      resolvablesMessageExpression(viewName, fieldName, contextName, bindVars))


  /** Returns resolvables expression for inclusion in resolver, where evaluation to not null enables resolve check.
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param bindVars    placeholder or variable expression strings
    */
  protected def resolvablesExpression(
      viewName: String, fieldName: String, contextName: String, bindVars: List[(String, Option[Type])]): String = {
    val hasPlaceholder = bindVars.find(_._1 == "_").isDefined
    def expr(v: String, typeOpt: Option[Type]) = {
      val cast = resolvableCastToText(typeOpt)
      if (v.startsWith(":") && v.endsWith("?"))
         s"if_defined_or_else($v, $v$cast, null)"
      else s"$v$cast"
    }
    if (hasPlaceholder)
      "_"
    else bindVars match {
      case Nil => expr("_", None)
      case List(v) => expr(v._1, v._2)
      case l => l.map(v => expr(v._1, v._2)).mkString("coalesce(", ", ", ")")
    }
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
    * @param resolvablesExpression
    * @param queryString
    * @param errorMessage
    */
  protected def resolverExpression(resolvablesExpression: String, queryString: String, errorMessage: String): String =
    s"checked_resolve($resolvablesExpression, array($queryString), $errorMessage)"

  protected def resolverExpression(
      viewName: String, fieldName: String, contextName: String, queryString: String, bindVars: List[String]): String = {
    val bindVarsTyped = bindVarsWithTypeOpt(viewName, fieldName, contextName, bindVars)
    val resolvables   = resolvablesExpression(viewName, fieldName, contextName, bindVarsTyped)
    val errorMessage  = resolverErrorMessageExpression(viewName, fieldName, contextName, bindVarsTyped)
    resolverExpression(resolvables, queryString, errorMessage)
  }

  // TODO to be fixed for nested resolvers?
  protected def bindVarsWithTypeOpt(
      viewName: String, fieldName: String, contextName: String, bindVars: List[String]): List[(String, Option[Type])] =
    viewDefOption(viewName).map { viewDef =>
      bindVars.map { name =>
        val varName = if (name == "_") Option(fieldName).map(":" + _).getOrElse("?") else name
        val v = parser.extractVariables(varName).head
        val path = (if (v.opt) v.copy(opt = false) else v).tresql.replaceAll("^:", "")
        val typeOpt = findField(viewDef, path).map(_.type_).filter(_.name != null)
        (name, typeOpt)
      }
    }.getOrElse(bindVars.map(_ -> None))

  def findField(viewDef: ViewDef, path: String): Option[FieldDef] = {
    if (SimpleIdentR.pattern.matcher(path).matches)
      Option(viewDef).flatMap(_.fieldOpt(path))
    else {
      val nameParts =
        parser.extractVariables(if (path == "?") path else ":" + path)
          .flatMap(v => v.variable :: v.members)
      val descViewDef =
        nameParts.dropRight(1).foldLeft(Option(viewDef)) {
          case (vOpt, n) =>
            vOpt flatMap { v =>
              findField(v, n)
                .filter(_.type_.isComplexType)
                .flatMap(f => viewDefOption(f.type_.name))
            }
        }.getOrElse(null)
      Option(descViewDef).flatMap(_.fieldOpt(nameParts.last))
    }
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
  def transformExpression(
      expression: String, viewDef: ViewDef, fieldDef: FieldDef, mdContext: MdContext,
      baseTableAlias: String = null, pathToAlias: Map[List[String], String] = null): String = {
    val fieldName = Option(fieldDef).map(_.fieldName).orNull
    val ctx = Context(viewDef, fieldDef, fieldName, baseTableAlias, pathToAlias, mdContext,
      RootCtx, addParensToSubquery = mdContext == Field)
    expressionTransformer(ctx)(
      parser.parseExp(expression)
    ).tresql
  }

  /** Used by expression transformer */
  protected def isResolverToBeTransformed(ctx: Context, q: org.tresql.ast.Query): Boolean = {
    ctx.mdContext == Resolver && ctx.transformerContext == RootCtx ||
    // OR
    ctx.transformerContext == EqOpCtx &&
    (ctx.mdContext == Filter || ctx.mdContext == Resolver) &&
    Option(q.cols).map(_.cols).filter(_ != null).getOrElse(Nil).size == 1
  }

  /** Returns expression transformer */
  protected def expressionTransformer: parser.TransformerWithState[Context] = parser.transformerWithState { ctx =>
    import parser._
    import org.tresql.parsing._
    import org.tresql.ast._
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
      val refFieldDef = refViewDef.fieldOpt(refFieldName)
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
      val tableDefOpt = Option(table).map(tableMetadata.tableDef(_, view.db))
      val key = tableDefOpt
        .map(_.pk).filter(_ != null).filter(_.isDefined).flatten
        .getOrElse(tableDefOpt
          .map(_.uk).filter(_ != null).map(_.filter(_ != null)).filter(_.nonEmpty).map(_.head)
          .orNull)
      val allRefs = tableDefOpt.map { tableDef =>
        if (table == refTable && key != null)
          tableDef.refs :+
            TableMetadata.Ref(
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
              traverser(usedNamesExtractor)(Set.empty)(parseExp(queryColExpr("true")))
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
        def withLimitQ(q: Query) =
          (if (q.limit == null) q.copy(limit = IntConst(2)) else q)
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
                .map {
                  // FIXME support any expression, allow alias
                  case IdentR(colName, _) =>
                    new org.mojoz.metadata.FieldDef(colName)
                  case colExpr =>
                    (new org.mojoz.metadata.FieldDef(name = null))
                      .copy(isExpression = true, expression = colExpr)
                }
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
              .filter(_ != "")
              .map { refFieldName =>
                refViewDef.fieldOpt(refFieldName)
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
          parseExp(resolvedQueryString) match {
            case q: Query => q
            case w: With => w
            case x =>
              sys.error("Unexpected query class: " + Option(x).map(_.getClass.getName).orNull +
                s" in $fullContextName parsed from: $resolvedQueryString")
          }
        }
        if (isResolverToBeTransformed(ctx, q)) {
          val resolverVars = traverser(placeholderAndVariableExtractor)(Nil)(q).reverse
          val optionalVarsSet = resolverVars.filter(_.opt).map(_.copy(opt = false).tresql).toSet
          val resolverVarsTresql = resolverVars.map {
            case Placeholder => "_"
            case v => if  (!v.opt)
                           v.tresql
                      else v.copy(opt = false).tresql
          }.distinct.map { case v if optionalVarsSet.contains(v) => v + "?" case v => v }
          val resolvableName = Option(ctx.fieldName).orElse(resolverVars.filter(_ != Placeholder).headOption.map(_.variable)).orNull
          val resolvedQueryStringWithLimit: String =
            resolvedQuery match {
              case q: Query => withLimitQ(q).tresql
              case w @ With(tables, q: Query) => w.copy(query = withLimitQ(q)).tresql
              case x =>
                sys.error("Unexpected query class: " + Option(x).map(_.getClass.getName).orNull +
                  s" in $fullContextName")
            }
          parseExp(resolverExpression(
            viewName, resolvableName, ctx.mdContext.name, resolvedQueryStringWithLimit, resolverVarsTresql
          ))
        } else if (ctx.addParensToSubquery) {
          parseExp("(" + resolvedQuery.tresql + ")")
        } else {
          resolvedQuery
        }
      case Ident(List(ident)) if ident == "^" =>
        ctx.mdContext match {
          case Filter =>
            if (ctx.viewDef.filter == null || ctx.viewDef.filter.isEmpty)
              parseExp("true")
            else {
              parseExp(
                transformFilter(
                  ctx.viewDef.filter.mkString("(", " & ", ")"),
                  ctx.viewDef, ctx.baseTableAlias, ctx.pathToAlias)
              )
            }
          case _ =>
            sys.error(s"Ref for ${ctx.mdContext} not supported yet") // TODO?
        }
      case iexpr @ Ident(List(ident)) if ident.startsWith("^") =>
        val name = ident.substring(1)
        lazy val pathToAlias = if (ctx.pathToAlias != null) ctx.pathToAlias else this.fromAndPathToAlias(ctx.viewDef)._2
        Option(ctx.viewDef)
          .flatMap(_.fieldOpt(name))
          .map(f => parseExp(Option(f.expression).map(_ => queryColExpression(ctx.viewDef, f, pathToAlias, null)).getOrElse(
             Option(f.tableAlias).orElse(Option(ctx.baseTableAlias)).map(_ + ".").getOrElse("") + f.name)))
          .getOrElse(iexpr)
      case iexpr @ Ident(identList: List[String]) if identList(0).startsWith("^") =>
        val ident = iexpr.tresql
        val (viewRef, dotRefFieldName) = ident.splitAt(ident.lastIndexOf("."))
        val (refViewName, refFieldName) = (viewRef.substring(1), dotRefFieldName.substring(1))
        val resolvedQueryString = refViewExpressionString(refViewName, refFieldName, refFilter = null)
        if (ctx.addParensToSubquery) {
          parseExp("(" + resolvedQueryString + ")")
        } else {
          parseExp(resolvedQueryString)
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
