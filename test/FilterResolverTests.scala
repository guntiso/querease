import org.scalatest.FlatSpec
import org.scalatest.Matchers

import querease.FilterResolver

class FilterResolverTests extends FlatSpec with Matchers {
  private val Identifiers = "blah some_ident1".split("\\s+").toSet
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  private val OtherFilters = Set("id = :id?", "a = b", "a = b & c = :d", "a<<")

  def resolve(f: String, b: String = null) = FilterResolver.resolve(f, b)
  private val b = "base"

  "filter sugar syntax" should "work properly for identifiers" in {
    Identifiers foreach { ident =>
      resolve(ident) should be(s"$ident = :$ident?")
      resolve(s"$ident!") should be(s"$ident = :$ident")
      resolve(s"$ident !") should be(s"$ident = :$ident")
      //
      resolve(ident, b) should be(s"base.$ident = :$ident?")
      resolve(s"$ident!", b) should be(s"base.$ident = :$ident")
      resolve(s"$ident !", b) should be(s"base.$ident = :$ident")
    }
  }

  "filter sugar syntax" should "handle naming correctly" in {
    resolve(s"blah <") should be(s"blah < :blah?")
    resolve(s"blah < !") should be(s"blah < :blah")
    resolve(s"a.blah <") should be(s"a.blah < :a_blah?")
    resolve(s"x_.blah <=") should be(s"x_.blah <= :blah?")
    resolve(s"x_.blah <=", "c") should be(s"x_.blah <= :blah?")
    resolve(s"blah <=", "c") should be(s"c.blah <= :blah?")
    resolve(s"blah <=", null) should be(s"blah <= :blah?")
  }

  "filter sugar syntax" should "work properly for comparisons" in {
    Identifiers foreach { ident =>
      ComparisonOps foreach { comp =>
        resolve(s"$ident $comp") should be(s"$ident $comp :$ident?")
        resolve(s"$ident $comp !") should be(s"$ident $comp :$ident")
        //
        resolve(s"$ident $comp", b) should be(s"base.$ident $comp :$ident?")
        resolve(s"$ident $comp !", b) should be(s"base.$ident $comp :$ident")
      }
    }
  }
/*
  "filter sugar syntax" should "work properly for wildcard comparisons" in {
    // %~ %~% %~~ %~~% ~% ~~% !%~ !%~% !%~~ !%~~% !~% !~~%
    Identifiers foreach { ident =>
      List("~", "~~") foreach { op =>
        List("", "!") foreach { not =>
          resolve(s"$ident $not%$op") should be(s"$ident $not$op '%' || :$ident?")
          resolve(s"$ident $not%$op !") should be(s"$ident $not$op '%' || :$ident")
          resolve(s"$ident $not$op%") should be(s"$ident $not$op :$ident? || '%'")
          resolve(s"$ident $not$op% !") should be(s"$ident $not$op :$ident || '%'")
          resolve(s"$ident $not%$op%") should be(s"$ident $not$op '%' || :$ident? || '%'")
          resolve(s"$ident $not%$op% !") should be(s"$ident $not$op '%' || :$ident || '%'")
          //
          resolve(s"$ident $not%$op", b) should be(s"base.$ident $not$op '%' || :$ident?")
          resolve(s"$ident $not%$op !", b) should be(s"base.$ident $not$op '%' || :$ident")
          resolve(s"$ident $not$op%", b) should be(s"base.$ident $not$op :$ident? || '%'")
          resolve(s"$ident $not$op% !", b) should be(s"base.$ident $not$op :$ident || '%'")
          resolve(s"$ident $not%$op%", b) should be(s"base.$ident $not$op '%' || :$ident? || '%'")
          resolve(s"$ident $not%$op% !", b) should be(s"base.$ident $not$op '%' || :$ident || '%'")
        }
      }
    }
  }

  "filter sugar syntax" should "work properly for insensitive wildcard comparisons" in {
    // ~~~ !~~~ %~~~ %~~~% ~~~% !%~~~ !%~~~% !~~~%
    val op = "~~~"
    Identifiers foreach { ident =>
      List("", "!") foreach { not =>
        resolve(s"$ident $not$op") should be(s"${not}cmp_i_exact($ident, :$ident?)")
        resolve(s"$ident $not$op !") should be(s"${not}cmp_i_exact($ident, :$ident)")
        resolve(s"$ident $not%$op") should be(s"${not}cmp_i_end($ident, :$ident?)")
        resolve(s"$ident $not%$op !") should be(s"${not}cmp_i_end($ident, :$ident)")
        resolve(s"$ident $not$op%") should be(s"${not}cmp_i_start($ident, :$ident?)")
        resolve(s"$ident $not$op% !") should be(s"${not}cmp_i_start($ident, :$ident)")
        resolve(s"$ident $not%$op%") should be(s"${not}cmp_i($ident, :$ident?)")
        resolve(s"$ident $not%$op% !") should be(s"${not}cmp_i($ident, :$ident)")
        //
        resolve(s"$ident $not$op", b) should be(s"${not}cmp_i_exact(base.$ident, :$ident?)")
        resolve(s"$ident $not$op !", b) should be(s"${not}cmp_i_exact(base.$ident, :$ident)")
        resolve(s"$ident $not%$op", b) should be(s"${not}cmp_i_end(base.$ident, :$ident?)")
        resolve(s"$ident $not%$op !", b) should be(s"${not}cmp_i_end(base.$ident, :$ident)")
        resolve(s"$ident $not$op%", b) should be(s"${not}cmp_i_start(base.$ident, :$ident?)")
        resolve(s"$ident $not$op% !", b) should be(s"${not}cmp_i_start(base.$ident, :$ident)")
        resolve(s"$ident $not%$op%", b) should be(s"${not}cmp_i(base.$ident, :$ident?)")
        resolve(s"$ident $not%$op% !", b) should be(s"${not}cmp_i(base.$ident, :$ident)")
      }
    }
  }
*/
  "filter sugar syntax" should "work properly for intervals" in {
    Identifiers foreach { ident =>
      IntervalOps foreach { leftComp =>
        IntervalOps foreach { rightComp =>
          resolve(s"$leftComp $ident $rightComp") should be(
            s":${ident}_from? $leftComp $ident $rightComp :${ident}_to?")
          resolve(s"$leftComp $ident $rightComp !") should be(
            s":${ident}_from? $leftComp $ident $rightComp :${ident}_to")
          resolve(s"! $leftComp $ident $rightComp") should be(
            s":${ident}_from $leftComp $ident $rightComp :${ident}_to?")
          resolve(s"! $leftComp $ident $rightComp !") should be(
            s":${ident}_from $leftComp $ident $rightComp :${ident}_to")
          //
          resolve(s"$leftComp $ident $rightComp", b) should be(
            s":${ident}_from? $leftComp base.$ident $rightComp :${ident}_to?")
          resolve(s"$leftComp $ident $rightComp !", b) should be(
            s":${ident}_from? $leftComp base.$ident $rightComp :${ident}_to")
          resolve(s"! $leftComp $ident $rightComp", b) should be(
            s":${ident}_from $leftComp base.$ident $rightComp :${ident}_to?")
          resolve(s"! $leftComp $ident $rightComp !", b) should be(
            s":${ident}_from $leftComp base.$ident $rightComp :${ident}_to")
        }
      }
    }
  }
/*
  "filter sugar syntax resolver" should "bypass everything else" in {
    OtherFilters foreach { other =>
      resolve(other) should be(other)
      resolve(other, b) should be(other)
    }
  }
*/
}
