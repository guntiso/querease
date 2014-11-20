package querease;

object FilterResolver {
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_a-zA-z][_a-zA-Z0-9]*"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val s = "\\s*"
  private val Rq = "(!)?"
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
  private def opt(req: String) = if (req == "!") "" else "?"
  // TODO configure naming
  private def colName(name: String, baseTableAlias: String) =
    if (baseTableAlias == null || name.indexOf(".") > 0) name
    else s"$baseTableAlias.$name"
  private def parName(name: String) =
    (if (name.contains("_.")) name.substring(name.lastIndexOf("_.") + 2) else name)
      .replace(".", "_")
  def resolve(filter: String, baseTableAlias: String) = {
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
      // cmp_i(ename, ?)        
      case x => x
    }
  }
}
