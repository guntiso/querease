package querease;

object FilterResolver {
  // TODO resolve names like in views (maybe strip prefix etc.)
  private val ident = "[_a-zA-z][_a-zA-Z0-9]*"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val s = "\\s*"
  private val Rq = "(!)?"
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  private val WildCmpOps =
    "%~ %~% %~~ %~~% ~% ~~% !%~ !%~% !%~~ !%~~% !~% !~~%".split("\\s+").toSet
  private val InsensitiveCmpOps =
    "~~~ !~~~ %~~~ %~~~% ~~~% !%~~~ !%~~~% !~~~%".split("\\s+").toSet
  private def toGroup(p: String) = if (p startsWith "(") p else s"($p)"
  private def regex(patterns: String*) =
    ("^" + patterns.map(toGroup).mkString(s, s, s) + "$").r

  private val IdentFilterDef =
    regex(qualifiedIdent, Rq)
  private val ComparisonFilterDef = regex(
    qualifiedIdent, ComparisonOps.mkString("|"), Rq)
  private val WildCmpFilterDef = regex(
    qualifiedIdent, WildCmpOps.mkString("|"), Rq)
  private val InsensitiveCmpFilterDef = regex(
    qualifiedIdent, InsensitiveCmpOps.mkString("|"), Rq)
  private val IntervalFilterDef = regex(Rq, IntervalOps.mkString("|"),
    qualifiedIdent, IntervalOps.mkString("|"), Rq)
  private def opt(req: String) = if (req == "!") "" else "?"
  // TODO configure naming
  private def colName(name: String, baseTableAlias: String) =
    if (baseTableAlias == null || name.indexOf(".") > 0) name
    else s"$baseTableAlias.$name"
  private def parName(name: String) = name.replace(".", "_")
  def resolve(filter: String, baseTableAlias: String) = {
    def par(name: String) = parName(name)
    def col(name: String) = colName(name, baseTableAlias)
    filter match {
      case IdentFilterDef(name, _, req) =>
        s"${col(name)} = :${par(name)}" + opt(req)
      case ComparisonFilterDef(name, _, op, req) =>
        s"${col(name)} $op :${par(name)}" + opt(req)
      case WildCmpFilterDef(name, _, op, req) =>
        val isNot = if (op startsWith "!") "!" else ""
        val wStart =
          if (op.startsWith("%") || op.startsWith("!%")) "'%' || " else ""
        val wEnd = if (op endsWith "%") " || '%'" else ""
        val cmp = if (op contains "~~") "~~" else "~"
        // TODO add wildcard escape function to generated string!
        s"${col(name)} $isNot$cmp $wStart:${par(name)}${opt(req)}$wEnd"
      case InsensitiveCmpFilterDef(name, _, op, req) =>
        val isNot = if (op startsWith "!") "!" else ""
        val wStart = op.startsWith("%") || op.startsWith("!%")
        val wEnd = op endsWith "%"
        val func = (wStart, wEnd) match {
          case (false, false) => "cmp_i_exact"
          case (false, true) => "cmp_i_start"
          case (true, false) => "cmp_i_end"
          case (true, true) => "cmp_i"
        }
        // TODO add wildcard escape function to generated string!
        s"$isNot$func(${col(name)}, :${par(name)}${opt(req)})"
      case IntervalFilterDef(reqFrom, opFrom, name, _, opTo, reqTo) =>
        val nameFrom = par(name + "_from") // TODO configure naming
        val nameTo = par(name + "_to") // TODO configure naming
        s":$nameFrom${opt(reqFrom)} $opFrom ${col(name)} $opTo :$nameTo${opt(reqTo)}"
      // cmp_i(ename, ?)        
      case x => x
    }
  }
}
