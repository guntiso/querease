package querease

import org.tresql._
import scala.language.postfixOps

class QuereaseMacros extends Macros {

  val hasNonAscii = """[^\p{ASCII}]"""r
  val hasUpper = """\p{javaUpperCase}"""r

  def ~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, false, false, true)
  def %~(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, false, true, false)
  def %~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, false, true, true)
  def !~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, true, false, true)
  def !%~(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, true, true, false)
  def !%~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, false, true, true, true)

  def ~~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, false, false, true)
  def %~~(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, false, true, false)
  def %~~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, false, true, true)
  def !~~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, true, false, true)
  def !%~~(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, true, true, false)
  def !%~~%(b: QueryBuilder, lop: Expr, rop: Expr) = matchMode(b, lop, rop, true, true, true, true)

  def ~~~(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, false, false, false)
  def ~~~%(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, false, false, true)
  def %~~~(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, false, true, false)
  def %~~~%(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, false, true, true)
  def !~~~(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, true, false, false)
  def !~~~%(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, true, false, true)
  def !%~~~(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, true, true, false)
  def !%~~~%(b: QueryBuilder, lop: Expr, rop: Expr) = resolve(b, lop, rop, true, true, true)

  private def resolve(b: QueryBuilder, lop: Expr, rop: Expr, not: Boolean, prefix: Boolean, suffix: Boolean) = {
    val (lopt, ropt) = maybeTransform(b, lop, rop)
    matchMode(b, lopt, ropt, false, not, prefix, suffix)
  }

  private def matchMode(b: QueryBuilder, lop: Expr, rop: Expr, ilike: Boolean, not: Boolean, prefix: Boolean, suffix: Boolean) = {
    val ropp = if (prefix) b.BinExpr("||", b.ConstExpr("%"), rop) else rop
    val rops = if (suffix) b.BinExpr("||", ropp, b.ConstExpr("%")) else ropp
    b.BinExpr((if (not) "!~" else "~") + (if (ilike) "~" else ""), lop, rops)
  }

  private def maybeTransform(b: QueryBuilder, lop: Expr, rop: Expr) = {
    val (unnacentL, lowerL) = valueProps(b, lop)
    val (unnacentR, lowerR) = valueProps(b, rop)
    val (lopu, ropu) =
      if (unnacentL || unnacentR) (b.FunExpr("unaccent", List(lop)), b.FunExpr("unaccent", List(rop)))
      else (lop, rop)
    val (lopl, ropl) =
      if (lowerL || lowerR) (b.FunExpr("lower", List(lopu)), b.FunExpr("lower", List(ropu)))
      else (lopu, ropu)
    (lopl, ropl)
  }

  private def valueProps(b: QueryBuilder, e: Expr) = {
    var hasVar = false
    var una = true
    var low = true
    b.transform(e, {
      case v: b.VarExpr => v() match {
        case x =>
          hasVar = true
          if (una) una = shouldUnaccent(String.valueOf(x))
          if (low) low = shouldIgnoreCase(String.valueOf(x))
          v
      }
    })
    (una && hasVar, low && hasVar)
  }

  protected def shouldUnaccent(s: String) = hasNonAscii.findFirstIn(s).isEmpty
  protected def shouldIgnoreCase(s: String) = hasUpper.findFirstIn(s).isEmpty

}