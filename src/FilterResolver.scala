package querease;

import mojoz.metadata._
import org.tresql.{ CacheBase, SimpleCacheBase }
import org.tresql.parsing.{ QueryParsers, ExpTransformer }
import scala.util.parsing.input.CharSequenceReader

object FilterTransformer {
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

import FilterTransformer._
trait FilterTransformer {
  val parser: Parser = DefaultParser
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val ident2 = s"$ident\\.$ident"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val qualifiedIdentOrRef = "\\^?" + qualifiedIdent
  private val s = "\\s*"
  private val any = ".*"
  private val Rq = "(!)?"
  private val Opt = "(\\?)?"
  private val choiceRef = ":\\^" + ident2
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "!?in\\b|[<>=!~%$]+"
  private def toGroup(p: String) = if (p startsWith "(") p else s"($p)"
  private def regex(patterns: String*) =
    ("^" + patterns.map(toGroup).mkString(s, s, s) + "$").r

  private val IdentFilterDef =
    regex(qualifiedIdentOrRef, Rq)
  private val ComparisonFilterDef = regex(
    qualifiedIdentOrRef, ComparisonOps, Rq)
  private val IntervalFilterDef = regex(Rq, IntervalOps.mkString("|"),
    qualifiedIdentOrRef, IntervalOps.mkString("|"), Rq)
  // ! ChoiceRefExpr syntax is experimental
  private val ChoiceRefExpr = regex(qualifiedIdent, "=", choiceRef, Opt)
  private def opt(req: String) = if (req == "!") "" else "?"
  // TODO configure naming
  private def colName(name: String, baseTableAlias: String) =
    if (baseTableAlias == null || name.indexOf(".") > 0 || name.startsWith("^")) name
    else s"$baseTableAlias.$name"
  private def parName(name: String) =
    (if (name.contains("_.")) name.substring(name.lastIndexOf("_.") + 2)
     else if (name startsWith "^") name.substring(1)
     else name)
      .replace(".", "_")
  def transformFilter(filter: String, view: ViewDef.ViewDefBase[FieldDef.FieldDefBase[Type]], baseTableAlias: String) = {
    def par(name: String) = parName(name)
    def col(name: String) = resolveFieldRefs(colName(name, baseTableAlias))
    def resolveFieldRefs(filterExpr: String): String = {
      import parser._
      transformer {
        case iexpr @ Ident(List(ident)) =>
          if (ident startsWith "^") {
            val name = ident.substring(1)
            Seq(view).filter(_ != null)
              .flatMap(_.fields)
              .find(f => Option(f.alias).getOrElse(f.name) == name)
              .map(f => parse(Option(f.expression).getOrElse(
                 Option(f.tableAlias).orElse(Option(baseTableAlias)).map(_ + ".").getOrElse("") + f.name)))
              .getOrElse(iexpr)
          } else iexpr
      } (parse(filterExpr)).tresql
    }
    filter match {
      case "true" | "false" =>
        filter
      case IdentFilterDef(name, _, req) =>
        s"${col(name)} = :${par(name)}" + opt(req)
      case ComparisonFilterDef(name, _, op, req) =>
        s"${col(name)} $op :${par(name)}" + opt(req)
      case IntervalFilterDef(reqFrom, opFrom, name, _, opTo, reqTo) =>
        val nameFrom = par(name + "_from") // TODO configure naming
        val nameTo = par(name + "_to") // TODO configure naming
        s":$nameFrom${opt(reqFrom)} $opFrom ${col(name)} $opTo :$nameTo${opt(reqTo)}"
      case ChoiceRefExpr(name, _, _, choiceRef, opt) =>
        // TODO ChoiceRefExpr syntax is experimental, not implemented here yet
        s"/* TODO - CHOICE REF FOUND - $name, $choiceRef, $opt */true"
      case x => resolveFieldRefs(x)
    }
  }
}

@deprecated("use FilterTransformer", "querease 1.4-SNAPSHOT")
object FilterResolver extends FilterTransformer {
  def resolve(filter: String, baseTableAlias: String) =
    transformFilter(filter, null, baseTableAlias)
}
