package querease;

object FilterResolver {
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_a-zA-z][_a-zA-Z0-9]*"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val s = "\\s*"
  private val Rq = "(!)?"
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet

  private def toGroup(p: String) = if (p startsWith "(") p else s"($p)"
  private def regex(patterns: String*) =
    ("^" + patterns.map(toGroup).mkString(s, s, s) + "$").r

  private val IdentFilterDef =
    regex(qualifiedIdent, Rq)
  private val ComparisonFilterDef = regex(
    qualifiedIdent, ComparisonOps.mkString("|"), Rq)
  private val IntervalFilterDef = regex(Rq, IntervalOps.mkString("|"),
    qualifiedIdent, IntervalOps.mkString("|"), Rq)
  private def opt(req: String) = if (req == "!") "" else "?"
  // TODO configure naming
  private def parName(name: String) = name.replace(".", "_")
  def resolve(filter: String) = filter match {
    case IdentFilterDef(name, _, req) =>
      s"$name = :${parName(name)}" + opt(req)
    case ComparisonFilterDef(name, _, op, req) =>
      s"$name $op :${parName(name)}" + opt(req)
    case IntervalFilterDef(reqFrom, opFrom, name, _, opTo, reqTo) =>
      val nameFrom = parName(name + "_from") // TODO configure naming
      val nameTo = parName(name + "_to") // TODO configure naming
      s":$nameFrom${opt(reqFrom)} $opFrom $name $opTo :$nameTo${opt(reqTo)}"
    case x => x
  }
}
