package querease;

import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.Type

import org.tresql.{ CacheBase, SimpleCacheBase }
import org.tresql.parsing.{ QueryParsers, ExpTransformer }
import scala.util.parsing.input.CharSequenceReader

trait FilterTransformer { this: QuereaseExpressions =>
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
  def transformFilter(filter: String, view: ViewDefBase[FieldDefBase[Type]], baseTableAlias: String) = {
    def par(name: String) = parName(name)
    def col(name: String) = colName(name, baseTableAlias)
    val transformedFilter = filter match {
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
      case x => x
    }
    transformExpression(transformedFilter, view, null: String, "filter", baseTableAlias)
  }
}
