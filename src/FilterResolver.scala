package querease;

import mojoz.metadata._

trait FilterTransformer {
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val ident2 = s"$ident\\.$ident"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val fieldRef = "\\^" + ident
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
    regex(qualifiedIdent, Rq)
  private val ComparisonFilterDef = regex(
    qualifiedIdent, ComparisonOps, Rq)
  private val IntervalFilterDef = regex(Rq, IntervalOps.mkString("|"),
    qualifiedIdent, IntervalOps.mkString("|"), Rq)
  private val RefExpr = regex(fieldRef, any) // FIXME support ref[s] at any position
  // ! ChoiceRefExpr syntax is experimental
  private val ChoiceRefExpr = regex(qualifiedIdent, "=", choiceRef, Opt)
  private def opt(req: String) = if (req == "!") "" else "?"
  // TODO configure naming
  private def colName(name: String, baseTableAlias: String) =
    if (baseTableAlias == null || name.indexOf(".") > 0) name
    else s"$baseTableAlias.$name"
  private def parName(name: String) =
    (if (name.contains("_.")) name.substring(name.lastIndexOf("_.") + 2) else name)
      .replace(".", "_")
  def transformFilter(filter: String, view: ViewDef.ViewDefBase[FieldDef.FieldDefBase[Type]], baseTableAlias: String) = {
    def par(name: String) = parName(name)
    def col(name: String) = colName(name, baseTableAlias)
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
      case RefExpr(refIdent, expr) =>
        // very basic support for field ref
        // FIXME support all shortcuts above
        // FIXME support ref[s] at any position
        val name = refIdent.substring(1)
        view.fields
          .find(f => Option(f.alias).getOrElse(f.name) == name)
          .map(f => Option(f.expression).getOrElse(f.name) + expr)
          .getOrElse(filter)
      case ChoiceRefExpr(name, _, _, choiceRef, opt) =>
        // TODO ChoiceRefExpr syntax is experimental, not implemented here yet
        s"/* TODO - CHOICE REF FOUND - $name, $choiceRef, $opt */true"
      case x => x
    }
  }
}

@deprecated("use FilterTransformer", "querease 1.4-SNAPSHOT")
object FilterResolver extends FilterTransformer {
  def resolve(filter: String, baseTableAlias: String) =
    transformFilter(filter, null, baseTableAlias)
}
