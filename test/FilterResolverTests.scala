package test

import mojoz.metadata._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.Seq

import querease.FilterTransformer

class FilterResolverTests extends FlatSpec with Matchers {
  private val Identifiers = "blah some_ident1".split("\\s+").toSet
  private val FieldRefs = "^blah".split("\\s+").toSet
  private val IntervalOps = "< <=".split("\\s+").toSet
  private val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  private val OtherFilters = Set("id = :id?", "a = b", "a = b & c = :d", "a<<")

  val transformer = new FilterTransformer {}
  def resolve(f: String, b: String = null) = transformer.transformFilter(f, null, b)
  def resolveBlah(f: String, b: String = null) = transformer.transformFilter(f, viewBlah, b)
  def resolveBlahA(f: String, b: String = null) = transformer.transformFilter(f, viewBlahA, b)
  def resolveBlahExpr(f: String, b: String = null) = transformer.transformFilter(f, viewBlahExpr, b)
  private val b = "base"
  private val f: FieldDef[Type] =
    FieldDef(
      table = null,
      tableAlias = null,
      name = null,
      alias = null,
      options = null,
      isCollection = false,
      maxOccurs = null,
      isExpression = false,
      expression = null,
      saveTo = null,
      resolver = null,
      nullable = true,
      default = null,
      isForcedCardinality = false,
      type_ = null,
      enum = null,
      joinToParent = null,
      orderBy = null,
      isI18n = false,
      comments = null,
      extras = null
    )
  private val v: ViewDef[FieldDef[Type]] =
    ViewDef(
      name = null,
      table = null,
      tableAlias = null,
      joins = Nil, // from clause
      filter = Nil, // where clause
      groupBy = Nil,
      having = Nil,
      orderBy = Nil,
      extends_  = null,
      draftOf = null,
      detailsOf = null,
      comments = null,
      fields = Nil,
      saveTo = Nil,
      extras = Map.empty
    )
  val fBlah = f.copy(name = "blah")
  val fBlahA = f.copy(name = "blah", tableAlias = "ba")
  val fBlahExpr = fBlah.copy(isExpression = true, expression = "to_date('1984')")
  val viewBlah = v.copy(fields = Seq(fBlah))
  val viewBlahA = v.copy(fields = Seq(fBlahA))
  val viewBlahExpr = v.copy(fields = Seq(fBlahExpr))



  "filter sugar syntax" should "work properly for identifiers" in {
    Identifiers foreach { ident =>
      resolve(ident) should be(s"$ident = :$ident?")
      resolve(s"$ident!") should be(s"$ident = :$ident")
      resolve(s"$ident !") should be(s"$ident = :$ident")
      //
      resolve(ident, b) should be(s"base.$ident = :$ident?")
      resolve(s"$ident!", b) should be(s"base.$ident = :$ident")
      resolve(s"$ident !", b) should be(s"base.$ident = :$ident")
      //
      resolve("true") should be("true")
      resolve("false") should be("false")
    }
  }

  "filter sugar syntax" should "work properly for missing field refs" in {
    FieldRefs foreach { fieldRef =>
      val ident = fieldRef.substring(1)
      resolve(fieldRef) should be(s"$fieldRef = :$ident?")
      resolve(s"$fieldRef!") should be(s"$fieldRef = :$ident")
      resolve(s"$fieldRef !") should be(s"$fieldRef = :$ident")
      //
      resolve(fieldRef, b) should be(s"$fieldRef = :$ident?")
      resolve(s"$fieldRef!", b) should be(s"$fieldRef = :$ident")
      resolve(s"$fieldRef !", b) should be(s"$fieldRef = :$ident")
    }
  }

  "filter sugar syntax" should "work properly for valid field refs" in {
    FieldRefs foreach { fieldRef =>
      val ident = fieldRef.substring(1)
      resolveBlah(fieldRef) should be(s"$ident = :$ident?")
      resolveBlah(s"$fieldRef!") should be(s"$ident = :$ident")
      resolveBlah(s"$fieldRef !") should be(s"$ident = :$ident")
      //
      resolveBlah(fieldRef, b) should be(s"base.$ident = :$ident?")
      resolveBlah(s"$fieldRef!", b) should be(s"base.$ident = :$ident")
      resolveBlah(s"$fieldRef !", b) should be(s"base.$ident = :$ident")
      //
      resolveBlahA(fieldRef) should be(s"ba.$ident = :$ident?")
      resolveBlahA(s"$fieldRef!") should be(s"ba.$ident = :$ident")
      resolveBlahA(s"$fieldRef !") should be(s"ba.$ident = :$ident")
      //
      resolveBlahA(fieldRef, b) should be(s"ba.$ident = :$ident?")
      resolveBlahA(s"$fieldRef!", b) should be(s"ba.$ident = :$ident")
      resolveBlahA(s"$fieldRef !", b) should be(s"ba.$ident = :$ident")
    }
  }

  "filter sugar syntax" should "work properly for valid field refs with expression" in {
    val expr = fBlahExpr.expression
    FieldRefs foreach { fieldRef =>
      val ident = fieldRef.substring(1)
      resolveBlahExpr(fieldRef) should be(s"$expr = :$ident?")
      resolveBlahExpr(s"$fieldRef!") should be(s"$expr = :$ident")
      resolveBlahExpr(s"$fieldRef !") should be(s"$expr = :$ident")
      //
      resolveBlahExpr(fieldRef, b) should be(s"$expr = :$ident?")
      resolveBlahExpr(s"$fieldRef!", b) should be(s"$expr = :$ident")
      resolveBlahExpr(s"$fieldRef !", b) should be(s"$expr = :$ident")
    }
  }

  "expression substitution" should "work properly for field refs" in {
    def filterExpr(s: String) = s"transform_me($s) = :'mypar'"
    val fieldName = fBlah.name
    val fieldRef = FieldRefs.head
    val fieldExpr = fBlahExpr.expression

    resolve(filterExpr(fieldRef)) should be(filterExpr(fieldRef))
    resolveBlah(filterExpr(fieldRef)) should be(filterExpr(fieldName))
    resolveBlahExpr(filterExpr(fieldRef)) should be(filterExpr(fieldExpr))
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
