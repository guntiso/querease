import org.scalatest.FlatSpec
import org.scalatest.Matchers

import querease.FilterResolver.resolve

class FilterResolverTests extends FlatSpec with Matchers {
  private val Identifiers = "blah some_ident1".split("\\s+").toSet
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  private val OtherFilters = Set("id = :id?", "a = b", "a = b & c = :d", "a<<")

  "filter sugar syntax" should "work properly for identifiers" in {
    Identifiers foreach { ident =>
      resolve(ident) should be(s"$ident = :$ident?")
      resolve(s"$ident!") should be(s"$ident = :$ident")
      resolve(s"$ident !") should be(s"$ident = :$ident")
    }
  }

  "filter sugar syntax" should "work properly for comparisons" in {
    Identifiers foreach { ident =>
      ComparisonOps foreach { comp =>
        resolve(s"$ident $comp") should be(s"$ident $comp :$ident?")
        resolve(s"$ident $comp !") should be(s"$ident $comp :$ident")
      }
    }
  }

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
        }
      }
    }
  }

  "filter sugar syntax resolver" should "bypass everything else" in {
    OtherFilters foreach { other =>
      resolve(other) should be(other)
    }
  }
}
