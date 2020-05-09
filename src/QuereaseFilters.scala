package querease

import scala.collection.immutable.Seq

trait FilterType
object FilterType {
  case class BooleanFilter(filter: String) extends FilterType
  case class IdentFilter(
    col: String, name: String, opt: String
  ) extends FilterType
  case class ComparisonFilter(
    col: String, operator: String, name: String, opt: String
  ) extends FilterType
  case class IntervalFilter(
    leftName: String, leftOpt: String, leftOperator: String,
    col: String,
    rightOperator: String, rightName: String, rightOpt: String
  ) extends FilterType
  // col = ^refViewName[^refFieldName = :name?]{refCol}
  case class RefFilter(
    col: String, name: String, opt: String,
    refViewName: String, refFieldName: String, refCol: String
  ) extends FilterType
  case class OtherFilter(filter: String) extends FilterType
}

trait FilterTransformer { this: Querease =>
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val ident2 = s"$ident\\.$ident"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val qualifiedIdentOrRef = "\\^?" + qualifiedIdent
  private val param = s":($ident)"
  private val s = "\\s*"
  private val any = ".*"
  private val Rq = "(!)?"
  private val Opt = "(\\?)?"
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "!?in\\b|[<>=!~%$]+"
  private def toGroup(p: String) = if (p startsWith "(") p else s"($p)"
  private def regex(patterns: String*) =
    ("^" + patterns.map(toGroup).mkString(s, s, s) + "$").r

  private val IdentFilterDef =
    regex(qualifiedIdentOrRef, Rq)
  private val ComparisonFilterDef = regex(
    qualifiedIdentOrRef, ComparisonOps, Rq)
  private val ComparisonFullFilterDef = regex(
    qualifiedIdentOrRef, ComparisonOps, param, Opt)
  private val IntervalFilterDef = regex(Rq, IntervalOps.mkString("|"),
    qualifiedIdentOrRef, IntervalOps.mkString("|"), Rq)
  private val RefFilterDef =
    // col = ^refViewName[^refFieldName = :name?]{refCol}
    s"^($qualifiedIdent)\\s*=\\s*\\^($ident)\\s*\\[\\s*\\^($ident)\\s*=\\s*:($ident)(\\?)?\\s*\\]\\s*\\{\\s*($qualifiedIdent)\\s*\\}$$".r
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
  def transformFilter(filter: String, view: ViewDef, baseTableAlias: String, pathToAlias: Map[List[String], String] = null): String = {
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
    transformExpression(transformedFilter, view, null, QuereaseExpressions.Filter, baseTableAlias, pathToAlias)
  }
  // TODO filter analyzer tests
  def analyzeFilter(filter: String, view: ViewDef, baseTableAlias: String): Seq[FilterType] = {
    import FilterType._
    def par(name: String) = parName(name)
    def col(name: String) = colName(name, baseTableAlias)
    val filterType = filter match {
      case "true" | "false" =>
        BooleanFilter(filter)
      case IdentFilterDef(name, _, req) =>
        IdentFilter(col(name), par(name), opt(req))
      case ComparisonFilterDef(name, _, op, req) =>
        ComparisonFilter(col(name), op, par(name), opt(req))
      case ComparisonFullFilterDef(name, _, op, _, param, o) =>
        ComparisonFilter(col(name), op, param, if (o == "?") "?" else "")
      case IntervalFilterDef(reqFrom, opFrom, name, _, opTo, reqTo) =>
        val nameFrom = par(name + "_from") // TODO configure naming
        val nameTo = par(name + "_to") // TODO configure naming
        IntervalFilter(nameFrom, opt(reqFrom), opFrom, col(name), opTo, nameTo, opt(reqTo))
      case RefFilterDef(col, _, refViewName, refFieldName, name, o, refCol, _) =>
        RefFilter(col, name, if (o == "?") "?" else "", refViewName, refFieldName, refCol)
      case x => OtherFilter(x)
    }
    // TODO analyze deeper levels
    List(filterType)
  }
}
