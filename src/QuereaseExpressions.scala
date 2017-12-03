package querease;

import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.Type

import org.tresql.{ CacheBase, SimpleCacheBase }
import org.tresql.parsing.{ QueryParsers, ExpTransformer }
import scala.util.parsing.input.CharSequenceReader

object QuereaseExpressions {
  trait Parser extends QueryParsers with ExpTransformer {
    def parse(expr: String): Exp
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
          cache.map(_.put(expr, e))
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
  val parser: Parser = DefaultParser
  /** Returns error message expression string for resolver
    *
    * @param viewName    view name this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    */
  protected def resolverErrorMessageExpression(viewName: String, fieldName: String, contextName: String): String =
    s"""'Failed to identify value of "$fieldName" (from $viewName) - ' || _"""

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
  protected def resolverExpression(queryString: String, errorMessage: String): String =
    s"checked_resolve(_, array($queryString), $errorMessage)"

  /** Returns transformed expression
    *
    * @param expression  expression to be transformed
    * @param viewDef     view this expression is from
    * @param fieldDef    field this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param baseTableAlias base table alias for filter transformation
    */
  protected def transformExpression(
      expression: String, viewDef: ViewDefBase[FieldDefBase[Type]], fieldDef: FieldDefBase[Type], contextName: String,
      baseTableAlias: String = null): String = {
    val fieldName = Option(fieldDef).map(f => Option(f.alias).getOrElse(f.name)).orNull
    transformExpression(expression, viewDef, fieldName, contextName, baseTableAlias)
  }

  /** Returns transformed expression
    *
    * @param expression  expression to be transformed
    * @param viewDef     view this expression is from
    * @param fieldName   field name this expression is from or null
    * @param contextName "filter" or "resolver" for now
    * @param baseTableAlias base table alias for filter transformation
    */
  protected def transformExpression(
      expression: String, viewDef: ViewDefBase[FieldDefBase[Type]], fieldName: String, contextName: String,
      baseTableAlias: String): String = {
    val viewName = Option(viewDef).map(_.name).orNull
    import parser._
    lazy val expressionTransformer: Transformer /* -WithState? */ = transformer {
      case w: With =>
        w
      case initialQ @ Query(tables, filter, cols, group, order, offset, limit) =>
        val q = initialQ/*.copy(
          tables = initialQ.tables.map(t => t.copy(
            obj = expressionTransformer(t.obj),
            join = Option(t.join).map(_.copy(expr = expressionTransformer(t.join.expr))).orNull
          )),
          filter = initialQ.filter.copy(filters = initialQ.filter.filters.map(f =>
            f.copy(elements = f.elements.map(expressionTransformer)))),
          cols = initialQ.cols.copy(cols = initialQ.cols.cols.map(c =>
            c.copy(col = expressionTransformer(c.col))))
          // group, orer, offset, limit
        )
        */
        val resolvableVarOpt = traverser(variableExtractor)(Nil)(q).reverse.headOption
        val resolvableName = Option(fieldName).orElse(resolvableVarOpt.map(_.variable)).orNull

        val isViewRef = tables.size == 1 && tables.head.tresql.startsWith("^")
        val isResolver = // TODO query is resolver?
          (contextName == "filter" || contextName == "resolver") &&
          //resolvableVarOpt.isDefined && // has at least one variable to resolve TODO or underscore
          Option(cols).map(_.cols).filter(_ != null).getOrElse(Nil).size == 1
        def fullContextName =
          s"$contextName of ${viewName}${Option(fieldName).map("." + _).getOrElse("")}"
        def withLimitQ(q: Query) =
          (if (q.limit == null) q.copy(limit = Const(2)) else q).tresql
        // TODO transform all fieldrefs of this query
        if (isViewRef) {
          val refViewName = tables.head.tresql.substring(1)
          val refViewDef = viewDefOption(refViewName)
            .getOrElse{
              throw new RuntimeException(
                s"View $refViewName referenced from $fullContextName is not found")
            }
          // val alias = fieldAliasOrName
          val refViewDefBaseTableAlias = null // FIXME refViewDefBaseTableAlias?
          val colFields =
            Option(cols)
              .map(_.cols)
              .getOrElse(Nil)
              .map(_.tresql)
              .map(colName => new mojoz.metadata.FieldDef(colName))
              .map(_.copy(table = refViewDef.table, tableAlias = refViewDef.tableAlias))
          def fieldRefExtractor: Traverser[List[String]] = { fieldRefs => {
            case nestedQ: Query => fieldRefs
            case iexpr @ Ident(List(ident)) if ident startsWith "^" =>
              ident :: fieldRefs
            }
          }
          val fieldRefs = traverser(fieldRefExtractor)(Nil)(q).reverse
          val refFields = fieldRefs
            .map(_.substring(1))
            .map { refFieldName =>
              refViewDef.fields
                .filter(f => Option(f.alias).getOrElse(f.name) == refFieldName)
                .headOption
                .getOrElse {
                  throw new RuntimeException(
                    s"Field $refViewName.$refFieldName referenced from $fullContextName is not found")
                }
            }
          val transformedFilterString = transformFilter(q.filter.filters.map(_.tresql).mkString, refViewDef, refViewDefBaseTableAlias)
          val resolvedQueryString = queryString(refViewDef, colFields, refFields, transformedFilterString)
          if (isResolver) {
            val errorMessage = resolverErrorMessageExpression(viewName, resolvableName, contextName)
            transformer {
              case Ident(List("_")) if resolvableVarOpt.isDefined =>
                resolvableVarOpt.get
            } (parse(resolverExpression(resolvedQueryString, errorMessage)))
          } else {
            parse(resolvedQueryString)
          }
        } else if (isResolver) {
          val errorMessage = resolverErrorMessageExpression(viewName, resolvableName, contextName)
          parse(resolverExpression(withLimitQ(q), errorMessage))
        } else {
          q
        }
      case iexpr @ Ident(List(ident)) if ident.startsWith("^") => // FIXME needs current context view, so, transformWithState?
        val name = ident.substring(1)
        Seq(viewDef).filter(_ != null)
          .flatMap(_.fields)
          .find(f => Option(f.alias).getOrElse(f.name) == name)
          .map(f => parse(Option(f.expression).getOrElse(
             Option(f.tableAlias).orElse(Option(baseTableAlias)).map(_ + ".").getOrElse("") + f.name)))
          .getOrElse(iexpr)
    }
    expressionTransformer(parse(expression)).tresql
  }
}
