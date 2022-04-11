package test

import com.typesafe.config.ConfigFactory

import java.sql.{Connection, Timestamp}
import java.text.SimpleDateFormat
import java.util.Date
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.tresql._
import dto._
import org.mojoz.querease.{QuereaseMacros, TresqlMetadata, ValidationException, ValidationResult}
import QuereaseTests._
import org.scalatest.BeforeAndAfterAll
import org.tresql.macro_.TresqlMacroInterpolator


trait QuereaseDbTests extends FlatSpec with Matchers with BeforeAndAfterAll {
  import QuereaseDbTests.{dataPath, clearEnv, commit, loadSampleData, MainDb, ExtraDb, Env, Env2}
  implicit var resources: org.tresql.Resources = null

  def setEnv(db: String): Unit
  def createDbObjects(db: String): Unit
  def isDbAvailable: Boolean = true
  def dbName: String
  def interceptedSqlExceptionMessage[B](b: => B): String  = try {
    b
    throw new RuntimeException("Expected message not thrown")
  } catch {
    case ex: TresqlException => ex.getCause.getMessage
    case ex: java.sql.SQLException => ex.getMessage
  }
  override def beforeAll(): Unit = {
    super.beforeAll()
    setEnv(MainDb)
    setEnv(ExtraDb)
    createDbObjects(MainDb)
    createDbObjects(ExtraDb)
    loadSampleData
    resources = Env.withUpdatedExtra(ExtraDb)(_.withConn(Env2.conn)) // XXX detaches from ThreadLocal
  }

  override def afterAll(): Unit = {
    super.afterAll()
    try commit catch { case util.control.NonFatal(_) => } finally clearEnv
  }

  if (isDbAvailable) "querease" should s"interact with $dbName database properly" in {
    val bank = new BankListRow
    bank.code = "b1"
    bank.name = "Bank 1"
    qe.save(bank)
    bank.code = "b2"
    bank.name = "Bank 2"
    qe.save(bank)
    qe.countAll[BankListRow](null) should be(2)
    val b1 = qe.get[BankListRow](10000).get
    b1.name should be("Bank 1")
    val b2 = qe.get[BankListRow](10001).get
    b2.name should be("Bank 2")
    qe.list[BankListRow](null).head.id should be(10001)
    val banks = qe.list[BankListRow](null, orderBy = "id")
    banks(0).id should be(10000)
    banks(0).name should be("Bank 1")
    banks(1).id should be(10001)
    banks(1).code should be("b2")
    banks.size should be(2)
    qe.list[BankListRowWithFilter](null).size should be(1)
    qe.delete(b1)
    val banksAfter = qe.list[BankListRow](null)
    banksAfter.size should be(1)
    val name2 = "Bank 2 updated name"
    b2.name = name2
    qe.save(b2)
    qe.get[BankListRow](10001).get.name should be(name2)
    qe.save(bank)
    qe.save(bank)
    qe.list[BankListRow](null).size should be(3)
    qe.list[BankListRowWithGroup](null).size should be(2)
    val banksHv = qe.list[BankListRowWithHaving](null)
    banksHv.size should be(1)
    banksHv(0).total should be(2)

    def personInfoString(p: PersonInfo) = {
      import p.{name, surname, sex, mother_name, father_name, children,
        maternal_grandmother, maternal_grandfather, paternal_grandmother, paternal_grandfather}
      def sxName(m: String, f: String) = if (sex == "M") m else f
      def hasInfo(info: String*) = info.filter(_ != null).size > 0
      List(
        List(name, surname).filter(_ != null).mkString(" "),
        if (!hasInfo(father_name, mother_name)) null else
          List(
            Option(father_name).map(_ +
              (if (!hasInfo(paternal_grandfather, paternal_grandmother)) "" else
                List(paternal_grandfather, paternal_grandmother)
                  .filter(_ != null).mkString(" (son of ", " and ", ")"))),
            Option(mother_name).map(_ +
              (if (!hasInfo(maternal_grandfather, maternal_grandmother)) "" else
                List(maternal_grandfather, maternal_grandmother)
                  .filter(_ != null)
                  .mkString(" (daughter of ", " and ", ")"))))
            .flatten
            .mkString(sxName("son of ", "daughter of "), " and ", ""),
        children.map(_.name) match {
          case Nil => null
          case child :: Nil => s"${sxName("father", "mother")} of $child"
          case childList => s"${sxName("father", "mother")} of " + {
            childList.reverse match {
              case last :: other =>
                other.reverse.mkString(", ") + " and " + last
              case _ => sys.error("impossible")
            }
          }
        },if(p.father != null) s"father's full name ${p.father.name} ${p.father.surname}" else null)
        .filter(_ != null)
        .mkString(", ")
    }
    val expected = fileToString(dataPath + "/" + "persons-out.txt")
    val produced = qe.list[PersonInfo](null).map(personInfoString)
      .mkString("", "\n", "\n")
    if (expected != produced)
      toFile(dataPath + "/" + s"persons-out-$dbName-produced.txt", produced)
    expected should be(produced)

    def altToPersonInfo(a: PersonInfoAlt) = {
      val p = new PersonInfo
      p.name = a.name
      p.surname = a.surname
      p.sex = a.sex
      p.mother_name = a.mother_name
      p.father_name = a.father_name
      p.maternal_grandmother = a.maternal_grandmother
      p.maternal_grandfather = a.maternal_grandfather
      p.paternal_grandmother = a.paternal_grandmother
      p.paternal_grandfather = a.paternal_grandfather
      p.children = a.children.map(c => { val n = new PersonName; n.name = c.name; n })
      p.father = if(a.father != null) {val n = new PersonInfoFather; n.name = a.father.name; n.surname = a.father.surname; n}else null
      p
    }
    val producedAlt = qe.list[PersonInfoAlt](null)
      .map(altToPersonInfo).map(personInfoString)
      .mkString("", "\n", "\n")
    if (expected != producedAlt)
      toFile(dataPath + "/" + s"persons-out-$dbName-produced-alt.txt", producedAlt)
    expected should be(producedAlt)

    val siblingsExpected = fileToString(dataPath + "/" + "siblings-out.txt")
    val siblingsProduced =
      qe.list[Siblings](null)
        .map(s => List(s.sibling1, s.sibling2).filter(_ != null).mkString(", "))
        .mkString("", "\n", "\n")
    if (siblingsExpected != siblingsProduced)
      toFile(dataPath + "/" + s"siblings-out-$dbName-produced.txt", siblingsProduced)
    siblingsExpected should be(siblingsProduced)

    val siblingsProducedAlt =
      qe.list[SiblingsAlt](null)
        .map(s => List(s.sibling1, s.sibling2).filter(_ != null).mkString(", "))
        .mkString("", "\n", "\n")
    if (siblingsExpected != siblingsProducedAlt)
      toFile(dataPath + "/" + s"siblings-out-$dbName-produced-alt.txt", siblingsProducedAlt)
    siblingsExpected should be(siblingsProducedAlt)

    val expectedFatherTree = fileToString(dataPath + "/" + "father-tree-out.txt")
    def fatherTreeList(indent: String, ff: List[FatherTree], result: List[String]): List[String] = ff match {
      case Nil => result
      case (person :: tail) =>
        val row = (indent + person.name)
        fatherTreeList(indent, tail,
          fatherTreeList(indent + "  ", person.sons.toList, row :: result))
    }
    val producedFatherTree =
      fatherTreeList("", qe.list[FatherTree](null).toList, Nil)
        .reverse.mkString("\n")
    if (expectedFatherTree != producedFatherTree)
      toFile(dataPath + "/" + s"father-tree-out-$dbName-produced.txt", producedFatherTree)
    expectedFatherTree should be(producedFatherTree)

    val expectedForefathers = fileToString(dataPath + "/" + "forefathers-out.txt")
    val producedForefathers =
      qe.list[WithForefathers](null).toList.flatMap { p => List(
        p.full_name,
        s"  ${p.forefathers.map(_.full_name).mkString(", ")}"
      )}.filterNot(_.trim == "").mkString("\n")
    if (expectedForefathers != producedForefathers)
      toFile(dataPath + "/" + s"forefathers-out-$dbName-produced.txt", producedForefathers)
    expectedForefathers should be(producedForefathers)

    // filter resolver tests with optional bind variable
    qe.countAll[FilterWithResolverTest1](Map.empty) shouldBe 0
    qe.countAll[FilterWithResolverTest1](Map("mother" -> null)) shouldBe 0
    qe.countAll[FilterWithResolverTest1](Map("mother" -> ("Minna" + "Priedīte"))) shouldBe 1
    qe.countAll[FilterWithResolverTest1](Map("mother" -> ("Helēna" + "Stūrīte"))) shouldBe 6
    qe.list    [FilterWithResolverTest1](Map("mother" -> ("Helēna" + "Stūrīte"))).size shouldBe 6
    interceptedSqlExceptionMessage {
      qe.countAll[FilterWithResolverTest1](Map("mother" -> "dada"))
    } shouldBe """Failed to identify value of "mother" (from filter_with_resolver_test_1) - dada"""
    //
    qe.countAll[FilterWithResolverTest4](Map.empty) shouldBe 37
    qe.countAll[FilterWithResolverTest4](Map("mother" -> null)) shouldBe 0
    qe.countAll[FilterWithResolverTest4](Map("mother" -> ("Minna Priedīte (#1)"))) shouldBe 1
    qe.countAll[FilterWithResolverTest4](Map("mother" -> ("Helēna Stūrīte (#1)"))) shouldBe 6
    qe.list    [FilterWithResolverTest4](Map("mother" -> ("Helēna Stūrīte (#1)"))).size shouldBe 6
    interceptedSqlExceptionMessage {
      qe.countAll[FilterWithResolverTest4](Map("mother" -> "dada"))
    } shouldBe """Failed to identify value of "mother" (from filter_with_resolver_test_4) - dada"""

    // filter resolver tests with col expression etc
    qe.countAll[FilterWithResolverTest3A](Map("mother" -> null)) shouldBe 0
    qe.countAll[FilterWithResolverTest3A](Map("mother" -> ("Minna" + "Priedīte"))) shouldBe 1
    qe.countAll[FilterWithResolverTest3A](Map("mother" -> ("Helēna" + "Stūrīte"))) shouldBe 6
    qe.countAll[FilterWithResolverTest3B](Map("mother" -> null)) shouldBe 0
    qe.countAll[FilterWithResolverTest3B](Map("mother" -> ("Minna" + "Priedīte"))) shouldBe 1
    qe.countAll[FilterWithResolverTest3B](Map("mother" -> ("Helēna" + "Stūrīte"))) shouldBe 6

    // resolver tests with optional bind variable
    OptionalParamsResolverTest1.resolve_mother_id(null,       null, null      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(null,       null, None      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(null,       null, Some(null)) shouldBe  null
    OptionalParamsResolverTest1.resolve_mother_id(null,       null, Some(77)  ) shouldBe  77
    OptionalParamsResolverTest1.resolve_mother_id(null,       null, null      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(None,       null, null      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(Some(null), null, null      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(Some(1000), null, null      ) shouldBe  1000
    OptionalParamsResolverTest1.resolve_mother_id(null,       22,   null      ) shouldBe  22
    OptionalParamsResolverTest1.resolve_mother_id(None,       22,   null      ) shouldBe  22
    OptionalParamsResolverTest1.resolve_mother_id(Some(null), 22,   null      ) shouldBe -1
    OptionalParamsResolverTest1.resolve_mother_id(Some(1000), 22,   null      ) shouldBe  1000

    // resolver test with bind variable from substructure
    val acc = new AccountWithBank
    val accb = new AccountWithBankBank
    val bacNr = "123456789"
    val bacNr2 = "111-222"
    accb.code = "BNP"
    accb.country_code = "FR"
    accb.id = 10001
    acc.bank = accb
    acc.resolve_bank_id shouldBe accb.id
    AccountWithBank.resolve_bank_id(accb) shouldBe accb.id
    acc.billing_account = bacNr
    acc.last_modified = new Timestamp(System.currentTimeMillis)
    val accountId = qe.save(acc)
    accountId shouldBe 10004
    qe.get[AccountWithBank](accountId).get.bank.id shouldBe accb.id

    var child = new PersonWithComplexTypeResolvers1
    child.name    = "Some"
    child.surname = "Child"
    child.sex     = "M"
    child.toSaveableMap.filter(_._1 matches "^\\w+$").toMap shouldBe
      Map("id" -> null, "name" -> "Some", "surname" -> "Child", "sex" -> "M", "mother_id" -> null, "father_id" -> null)
    val childId = qe.save(child)
    childId shouldBe 10005
    child = qe.get[PersonWithComplexTypeResolvers1](childId).get
    child.mother shouldBe null
    child.father shouldBe null
    child.resolve_mother_id shouldBe null
    child.resolve_father_id shouldBe null
    var mother = new PersonWithComplexTypeResolvers1Mother
    mother.name    = "Some"
    mother.surname = "Mother"
    var father = new PersonWithComplexTypeResolvers1Father
    father.name    = "Some"
    father.surname = "Father"
    child.mother = mother
    child.father = father
    interceptedSqlExceptionMessage {
      child.resolve_father_id
    } shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - Some, Father"""
    interceptedSqlExceptionMessage {
      PersonWithComplexTypeResolvers1.resolve_father_id(child.father)
    } shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - Some, Father"""
    val motherId = qe.save(mother)
    motherId shouldBe 10006
    val fatherId = qe.save(father)
    fatherId shouldBe 10007
    PersonWithComplexTypeResolvers1.resolve_mother_id(mother) shouldBe motherId
    PersonWithComplexTypeResolvers1.resolve_father_id(father) shouldBe fatherId
    child.resolve_mother_id shouldBe motherId
    child.resolve_father_id shouldBe fatherId
    qe.save(child) shouldBe childId
    child = qe.get[PersonWithComplexTypeResolvers1](childId).get
    child.mother.surname shouldBe "Mother"
    child.mother.sex     shouldBe "F"
    child.father.surname shouldBe "Father"
    child.father.sex     shouldBe "M"
    child.resolve_mother_id shouldBe motherId
    child.resolve_father_id shouldBe fatherId

    val person9 = new ResolverTestPerson9A
    person9.name = "Some"
    interceptedSqlExceptionMessage {
      person9.resolve_mother_id(surname = "Mother", `type` = "crocodile")
    } shouldBe """Failed to identify value of "mother_id" (from resolver_test_person_9_a) - Some, Mother, crocodile"""
    person9.resolve_mother_id(surname = "Mother", `type` = "person") shouldBe motherId
    interceptedSqlExceptionMessage {
      ResolverTestPerson9A.resolve_mother_id(name = "Some", surname = "Mother", `type` = "crocodile")
    } shouldBe """Failed to identify value of "mother_id" (from resolver_test_person_9_a) - Some, Mother, crocodile"""
    ResolverTestPerson9A.resolve_mother_id("Some", "Mother", "person") shouldBe motherId
    ResolverTestPerson9A.resolve_mother_id(name = "Some", surname = "Mother", `type` = "person") shouldBe motherId
    ResolverTestPerson9B.resolve_mother_id(name = "Some", surname = "Mother", `creative param name` = "person") shouldBe motherId
    ResolverTestPerson9C.resolve_mother_id(name = "Some", surname = "Mother", `creative.param.name` = "person") shouldBe motherId

    qe.countAll[ResolverTestPerson10](Map("name" -> "Alfrēds")) shouldBe 1
    qe.countAll[ResolverTestPerson10](Map("name" -> "Marija" )) shouldBe 2
    ResolverTestPerson10.resolve_id("Alfrēds") shouldBe 1108
    interceptedSqlExceptionMessage {
      ResolverTestPerson10.resolve_id(name = "Marija")
    } shouldBe """Failed to identify value of "id" (from resolver_test_person_10) - Marija"""
    interceptedSqlExceptionMessage {
      ResolverTestPerson11.resolve_id(name = "Alfrēds")
    } shouldBe """Failed to identify value of "id" (from resolver_test_person_11) - Alfrēds"""
    ResolverTestPerson12A.resolve_father_id("Alfrēds", 1) shouldBe 1108
    ResolverTestPerson12B.resolve_father_id("Alfrēds", 1) shouldBe 1108
    interceptedSqlExceptionMessage {
      ResolverTestPerson12B.resolve_father_id("Alfrēds", 0)
    } shouldBe """Failed to identify value of "father" (from resolver_test_person_12_b) - Alfrēds"""
    ResolverTestPerson12C.resolve_father_id("Alfrēds", 1) shouldBe 1108
    interceptedSqlExceptionMessage {
      ResolverTestPerson12C.resolve_father_id("Alfrēds", 0)
    } shouldBe """Failed to identify value of "father" (from resolver_test_person_12_c) - Alfrēds, 0"""

    var child2 = qe.get[PersonWithComplexTypeResolvers2](childId).get
    child2.resolve_father_id(m => m + ("father" -> (m("father").asInstanceOf[Map[String, Any]] ++ Map("is_resolver_disabled" -> false)))) shouldBe fatherId
    interceptedSqlExceptionMessage {
      child2.resolve_father_id(m => m + ("father" -> (m("father").asInstanceOf[Map[String, Any]] ++ Map("is_resolver_disabled" -> true))))
    } shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - Some, Father, true"""
    child2.resolve_father_id(m => m) shouldBe fatherId

    val accountCurrency = new AccountCurrency
    accountCurrency.account_id = accountId
    accountCurrency.currency_code = "EUR"
    qe.save(accountCurrency)

    // dto resolver tests
    PersonChoiceResolverImplied.resolve_id("Guntis Ozols (#2)") shouldBe 1127
    interceptedSqlExceptionMessage {
      PersonChoiceResolverImplied.resolve_id("blah blah")
    } shouldBe """Failed to identify value of "full_name" (from person_choice_resolver_implied) - blah blah"""
    val personChoiceResolverImplied = new PersonChoiceResolverImplied
    personChoiceResolverImplied.full_name = "Andris Ozols (#2)"
    personChoiceResolverImplied.resolve_id shouldBe 1128
    interceptedSqlExceptionMessage {
      personChoiceResolverImplied.full_name = "dada dada"
      personChoiceResolverImplied.resolve_id
    } shouldBe """Failed to identify value of "full_name" (from person_choice_resolver_implied) - dada dada"""
    ResolverTestAccountCurrency1.resolve_account_id(bacNr) shouldBe accountId
    interceptedSqlExceptionMessage {
      ResolverTestAccountCurrency1.resolve_account_id(s"$bacNr-X")
    } shouldBe """Failed to identify value of "account" (from resolver_test_account_currency_1) - 123456789-X"""
    ResolverTestAccountCurrency1.resolve_currency_code("Euro") shouldBe "EUR"
    interceptedSqlExceptionMessage {
      ResolverTestAccountCurrency1.resolve_currency_code("Euro-X")
    } shouldBe """Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - Euro-X"""
    val resolverTestAccountCurrency1 = new ResolverTestAccountCurrency1
    resolverTestAccountCurrency1.account = bacNr
    resolverTestAccountCurrency1.currency_name = "Euro"
    resolverTestAccountCurrency1.resolve_account_id shouldBe accountId
    resolverTestAccountCurrency1.resolve_currency_code shouldBe "EUR"
    resolverTestAccountCurrency1.account += "-Z"
    resolverTestAccountCurrency1.currency_name += "-Z"
    interceptedSqlExceptionMessage {
      resolverTestAccountCurrency1.resolve_account_id
    } shouldBe """Failed to identify value of "account" (from resolver_test_account_currency_1) - 123456789-Z"""
    interceptedSqlExceptionMessage {
      resolverTestAccountCurrency1.resolve_currency_code
    } shouldBe """Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - Euro-Z"""

    // save with children test
    var bankWithAcc = qe.get[BankWithAccounts1](accb.id).get
    bankWithAcc.accounts(0).last_modified = null
    bankWithAcc.toMap shouldBe Map("id" -> accb.id, "code" -> "b2", "name" -> "Bank 2 updated name",
      "accounts" -> List(Map("id" -> accountId, "billing_account" -> bacNr, "last_modified" -> null)))
    bankWithAcc.accounts(0).billing_account = bacNr2
    qe.save(bankWithAcc) // child update
    qe.get[AccountWithBank](accountId).get.bank.id shouldBe accb.id
    qe.get[AccountWithBank](accountId).get.billing_account shouldBe bacNr2
    bankWithAcc = qe.get[BankWithAccounts1](accb.id).get
    bankWithAcc.accounts(0).last_modified = null
    bankWithAcc.toMap shouldBe Map("id" -> accb.id, "code" -> "b2", "name" -> "Bank 2 updated name",
      "accounts" -> List(Map("id" -> accountId, "billing_account" -> bacNr2, "last_modified" -> null)))
    bankWithAcc.accounts(0).billing_account = bacNr
    qe.save(bankWithAcc) // child update
    val acc2 = new BankWithAccounts1Accounts
    acc2.billing_account = bacNr2
    bankWithAcc.accounts = acc2 :: bankWithAcc.accounts
    qe.save(bankWithAcc) // child add
    qe.countAll[AccountWithBank](Map("bank_id" -> accb.id)) shouldBe 2
    bankWithAcc = qe.get[BankWithAccounts1](accb.id).get
    bankWithAcc.accounts = bankWithAcc.accounts.reverse.tail
    qe.save(bankWithAcc) // child delete
    qe.countAll[AccountWithBank](Map("bank_id" -> accb.id)) shouldBe 1
    bankWithAcc = qe.get[BankWithAccounts1](accb.id).get
    bankWithAcc.accounts(0).last_modified = null
    bankWithAcc.toMap shouldBe Map("id" -> accb.id, "code" -> "b2", "name" -> "Bank 2 updated name",
      "accounts" -> List(Map("id" -> accountId, "billing_account" -> bacNr, "last_modified" -> null)))

    // save with lookup tests
    var accwb2 = qe.get[AccountWithBank2](accountId).get
    accwb2.toSaveableMap shouldBe Map("id" -> accountId, "billing_account" -> bacNr,
      "bank_id" -> Map("id" -> accb.id, "code" -> "b2", "*country_code" -> null))
    accwb2.billing_account = bacNr2
    accwb2.bank.code = "b2-upd"
    qe.save(accwb2)
    accwb2 = qe.get[AccountWithBank2](accountId).get
    accwb2.toSaveableMap shouldBe Map("id" -> accountId, "billing_account" -> bacNr2,
      "bank_id" -> Map("id" -> accb.id, "code" -> "b2-upd", "*country_code" -> null))
    // save with lookup tests - auto-resolve ambiguity from alias
    var pwp = new PersonWithParents1
    pwp.name = "With"
    pwp.surname = "Parents"
    pwp.sex = "M"
    pwp.father = new PersonWithParents1Father
    pwp.father.name = "Father-With"
    pwp.father.surname = "Child"
    pwp.mother = new PersonWithParents1Mother
    pwp.mother.name = "Mother-With"
    pwp.mother.surname = "Child"
    val pwpId = qe.save(pwp)
    pwp = qe.get[PersonWithParents1](pwpId).get
    val pwpMotherId = pwp.mother.id
    val pwpFatherId = pwp.father.id
    pwp.mother.name shouldBe "Mother-With"
    pwp.mother.name = "Mother-With-Upd"
    qe.save(pwp)
    pwp = qe.get[PersonWithParents1](pwpId).get
    pwpMotherId shouldBe pwp.mother.id
    pwpFatherId shouldBe pwp.father.id
    pwp.mother.name shouldBe "Mother-With-Upd"
    pwp.father = null
    qe.save(pwp)
    qe.get[Person](pwpId).get.mother_id shouldBe pwpMotherId
    qe.get[Person](pwpId).get.father_id shouldBe null

    // no-id test
    val noid = new NoidTest
    noid.id = 0
    noid.nm = "name"
    qe.get[NoidTest](0).map(_.toMap).orNull shouldBe null
    qe.save(noid, forceInsert = true)
    qe.get[NoidTest](0).map(_.toMap).orNull shouldBe Map("id" -> 0, "nm" -> "name")
    noid.id = 1
    qe.save(noid, forceInsert = true)
    qe.get[NoidTest](1).map(_.toMap).orNull shouldBe Map("id" -> 1, "nm" -> "name")
    qe.list[NoidTest](null).map(_.toMap) shouldBe List(
      Map("id" -> 0, "nm" -> "name"),
      Map("id" -> 1, "nm" -> "name"),
    )
    noid.nm = "updated name"
    qe.save(noid)
    qe.get[NoidTest](1).map(_.toMap).orNull shouldBe Map("id" -> 1, "nm" -> "updated name")
    qe.list[NoidTest](null).map(_.toMap) shouldBe List(
      Map("id" -> 0, "nm" -> "name"),
      Map("id" -> 1, "nm" -> "updated name"),
    )
    qe.delete(noid)
    qe.get[NoidTest](1).map(_.toMap).orNull shouldBe null
    qe.list[NoidTest](null).map(_.toMap) shouldBe List(
      Map("id" -> 0, "nm" -> "name"),
    )
    noid.id = null
    noid.nm = "name"
    val noid_1 = qe.save(noid)
    qe.get[NoidTest](noid_1).map(_.toMap).orNull shouldBe Map("id" -> noid_1, "nm" -> "name")
    noid.id = noid_1
    noid.nm = "updated name"
    qe.save(noid)
    qe.list[NoidTest](null).map(_.toMap) shouldBe List(
      Map("id" -> 0,      "nm" -> "name"),
      Map("id" -> noid_1, "nm" -> "updated name"),
    )
    noid.id = 0
    qe.delete(noid)
    noid.id = noid_1
    qe.delete(noid)
    qe.list[NoidTest](null).map(_.toMap) shouldBe Nil

    // no-id test 2
    val noid2 = new NoidTest2
    noid2.no_id = 0
    noid2.no_nm = "name"
    qe.get[NoidTest2](0).map(_.toMap).orNull shouldBe null
    qe.save(noid2, forceInsert = true)
    qe.get[NoidTest2](0).map(_.toMap).orNull shouldBe Map("no_id" -> 0, "no_nm" -> "name")
    noid2.no_id = 1
    qe.save(noid2, forceInsert = true)
    qe.get[NoidTest2](1).map(_.toMap).orNull shouldBe Map("no_id" -> 1, "no_nm" -> "name")
    qe.list[NoidTest2](null).map(_.toMap) shouldBe List(
      Map("no_id" -> 0, "no_nm" -> "name"),
      Map("no_id" -> 1, "no_nm" -> "name"),
    )
    noid2.no_nm = "updated name"
    qe.save(noid2)
    qe.get[NoidTest2](1).map(_.toMap).orNull shouldBe Map("no_id" -> 1, "no_nm" -> "updated name")
    qe.list[NoidTest2](null).map(_.toMap) shouldBe List(
      Map("no_id" -> 0, "no_nm" -> "name"),
      Map("no_id" -> 1, "no_nm" -> "updated name"),
    )
    qe.delete(noid2)
    qe.get[NoidTest2](1).map(_.toMap).orNull shouldBe null
    qe.list[NoidTest2](null).map(_.toMap) shouldBe List(
      Map("no_id" -> 0, "no_nm" -> "name"),
    )
    noid2.no_id = null
    noid2.no_nm = "name"
    val noid2_1 = qe.save(noid2)
    qe.get[NoidTest2](noid2_1).map(_.toMap).orNull shouldBe Map("no_id" -> noid2_1, "no_nm" -> "name")
    noid2.no_id = noid2_1
    noid2.no_nm = "updated name"
    qe.save(noid2)
    qe.list[NoidTest2](null).map(_.toMap) shouldBe List(
      Map("no_id" -> 0,       "no_nm" -> "name"),
      Map("no_id" -> noid2_1, "no_nm" -> "updated name"),
    )
    qe.get[NoidTest2](0).map(_.toMap).orNull       shouldBe Map("no_id" -> 0,       "no_nm" -> "name")
    qe.get[NoidTest2](noid2_1).map(_.toMap).orNull shouldBe Map("no_id" -> noid2_1, "no_nm" -> "updated name")
    noid2.no_id = 0
    qe.delete(noid2)
    noid2.no_id = noid2_1
    qe.delete(noid2)
    qe.list[NoidTest2](null).map(_.toMap) shouldBe Nil

    var multiTest       = new SaveToMultiTest01
    multiTest.name      = "Multitest"
    multiTest.password  = "demo"
    val multiTestId = qe.save(multiTest)

    multiTest = qe.get[SaveToMultiTest01](multiTestId).get
    multiTest.id        shouldBe multiTestId
    multiTest.name      shouldBe "Multitest"
    multiTest.password  shouldBe "demo"

    multiTest.name      = "Multitest2"
    multiTest.password  = "demo2"
    qe.save(multiTest)

    multiTest = qe.get[SaveToMultiTest01](multiTestId).get
    multiTest.id        shouldBe multiTestId
    multiTest.name      shouldBe "Multitest2"
    multiTest.password  shouldBe "demo2"

    // save extra props test - to be remvoed
    val saveExtra      = new SaveExtraPropsTest01
    val saveExtraId    = qe.save(saveExtra, extraPropsToSave = Map("person_id" -> multiTest.id))
    tresql"sys_user[id = $saveExtraId] {person_id}".unique[Long] shouldBe multiTest.id
    tresql"sys_user[id = $saveExtraId] {password}".unique[String] shouldBe null
    saveExtra.id = saveExtraId
    qe.save(saveExtra, extraPropsToSave = Map("password" -> "demo3"))
    tresql"sys_user[id = $saveExtraId] {password}".unique[String] shouldBe "demo3"
  }

  if (isDbAvailable) it should s"validate in $dbName properly" in {
    val dto = new ValidationsTest

    dto.integer_column = 3
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(List(ValidationResult(Nil,
      List("integer_column should be greater than 5 but is 3", "integer_column should be greater than 10 but is 3")
    )))

    dto.integer_column = 7
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(List(ValidationResult(Nil,
      List("integer_column should be greater than 10 but is 7")
    )))

    dto.integer_column = 11
    qe.save(dto)

    dto.integer_column = 13
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(List(ValidationResult(Nil,
      List("integer_column should be less than 12 but is 13")
    )))

    dto.integer_column = 11
    val ch11 = new ValidationsTestChild1
    ch11.integer_column = 0
    val ch12 = new ValidationsTestChild1
    ch12.integer_column = 1
    val ch21 = new ValidationsTestChild2
    ch21.integer_column = 0
    val ch22 = new ValidationsTestChild2
    ch22.integer_column = 1
    dto.children1 = List(ch11, ch12)
    dto.children2 = List(ch21, ch22)
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(
      List(ValidationResult(List("children1", 0), List("child1 integer_column should be greater than 1 but is 0")),
        ValidationResult(List("children1", 1), List("child1 integer_column should be greater than 1 but is 1")),
        ValidationResult(List("children2", 0), List("child2 integer_column should be greater than 2 and parent must be greater than 3 but is 0,11")),
        ValidationResult(List("children2", 1), List("child2 integer_column should be greater than 2 and parent must be greater than 3 but is 1,11")))
    )

    dto.integer_column = 0
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(
      List(ValidationResult(Nil, List("integer_column should be greater than 5 but is 0",
        "integer_column should be greater than 10 but is 0",
        "Children integer_column field sum must be less than parent's integer_column value. Instead - 0 < 2")),
        ValidationResult(List("children1", 0), List("child1 integer_column should be greater than 1 but is 0")),
        ValidationResult(List("children1", 1), List("child1 integer_column should be greater than 1 but is 1")),
        ValidationResult(List("children2", 0), List("child2 integer_column should be greater than 2 and parent must be greater than 3 but is 0,0")),
        ValidationResult(List("children2", 1), List("child2 integer_column should be greater than 2 and parent must be greater than 3 but is 1,0")))
    )

    dto.integer_column = 11
    dto.children1(0).integer_column = 2
    dto.children1(1).integer_column = 2
    dto.children2(0).integer_column = 3
    dto.children2(1).integer_column = 3
    qe.save(dto)

    dto.integer_column = 11
    dto.children1(0).integer_column = 1
    dto.children2(1).integer_column = 2
    intercept[ValidationException] {
      qe.save(dto)
    }.details should be(
      List(ValidationResult(List("children1", 0), List("child1 integer_column should be greater than 1 but is 1")),
        ValidationResult(List("children2", 1), List("child2 integer_column should be greater than 2 and parent must be greater than 3 but is 2,11")))
    )
  }
  if (isDbAvailable) it should s"support multiple schemas in $dbName" in {
    val personCount = tresql"person{count(*)}".unique[Int]
    val carCount    = tresql"car_schema.person_car{count(*)}".unique[Int]
    ((1 to 10).map(i => f"person_and_car_$i%02d") ++
     (1 to 10).map(i => f"car_and_person_$i%02d")
    ) foreach { viewName =>
      val viewDef   = qe.viewDef(viewName)
      val (q, p)    = qe.queryStringAndParams(viewDef, Map.empty)
      try {
        val values  = Query(q, p).toListOfMaps
        val expectedSize = if (viewName startsWith "person") personCount + carCount - 1 else carCount
        values.size shouldBe expectedSize
        values.find(v => (v - "id") == Map("name" -> "Guntis", "car_name" -> "Tesla")).isDefined shouldBe true
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Schema support test failed for $viewName. Query string: $q", ex)
      }
    }
  }
  if (isDbAvailable) it should s"support multi-db views in $dbName" in {
    val personCount = tresql"person[id < 2000]{count(*)}".unique[Int]
    def isNameSurnameEqFullName(p1: Map[String, Any], p2: Map[String, Any]) =
      List(p1.getOrElse("name", "?"), p1.getOrElse("surname", "?")).mkString(" ") == p2.getOrElse("full_name", "??")
    def isSamePersonFromDifferentDatabases(p1: Map[String, Any], p2: Map[String, Any]) =
      isNameSurnameEqFullName(p1, p2) ||
      isNameSurnameEqFullName(p2, p1)
    def hasSamePersonFromDifferentDatabases(v: Map[String, Any]) = isSamePersonFromDifferentDatabases(
      v,
      v.getOrElse("other_db_person", Nil).asInstanceOf[List[Map[String, Any]]].headOption.getOrElse(Map.empty)
    )
    ((1 to 2).map(i => f"person_from_multiple_db_$i%d")) foreach { viewName =>
      val viewDef   = qe.viewDef(viewName)
      val (q, p)    = qe.queryStringAndParams(viewDef, Map.empty)
      try {
        val values  = Query(q, p).toListOfMaps
        val expectedSize = personCount
        values.size shouldBe expectedSize
        values.find(hasSamePersonFromDifferentDatabases).isDefined shouldBe true
        values.count(hasSamePersonFromDifferentDatabases) should be < personCount
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Multi-db support test failed for $viewName. Query string: $q", ex)
      }
    }
    // test children save to extra db
    def currentAccountsCount = Query("|querease2:account{count(*)}").unique[Int]
    currentAccountsCount shouldBe 0
    val pa = new PersonAccounts2
    pa.id = 1100
    pa.accounts = List(
      new PersonAccounts2Accounts,
      new PersonAccounts2Accounts,
    )
    qe.save(pa)
    currentAccountsCount shouldBe 2
    pa.accounts = List(
      new PersonAccounts2Accounts,
    )
    qe.save(pa)
    currentAccountsCount shouldBe 1
    // test save / delete to extra db
    val p2 = new Person2
    p2.full_name = "Test Only"
    val newId = qe.save(p2)
    val p2New = qe.get[Person2](newId).get
    qe.delete(p2New)
    qe.get[Person2](newId).isEmpty shouldBe true

    //test list with macro
    qe.list[Person2](Map[String, Any](), orderBy = "id").head.notes shouldBe("no_args")
    tresql"{ no_args_macro() x }".map(_.x).toList.head shouldBe("no_args")
  }

  if (isDbAvailable) it should s"crud by keys in $dbName" in {
    val org = new OrganizationKeyTest
    var org_saved: OrganizationKeyTest = null
    val org_main_account = new OrganizationKeyTestMainAccount

    org.name = "org"
    org_main_account.number = "A1"
    org_main_account.balance = 100
    org.main_account = org_main_account
    qe.save(org, forceInsert = true)

    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.name shouldBe "org"
    org_saved.main_account.number shouldBe "A1"
    org_saved.main_account.balance shouldBe 100
    org_main_account.balance = 200
    qe.save(org)

    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.name shouldBe "org"
    org_saved.main_account.number shouldBe "A1"
    org_saved.main_account.balance shouldBe 200

    val a2 = new OrganizationKeyTestAccounts
    a2.number = "A2"
    a2.balance = 2
    val a3 = new OrganizationKeyTestAccounts
    a3.number = "A3"
    a3.balance = 3

    org.accounts = List(a2)
    qe.save(org)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe org.accounts.map(_.toMap)

    org.accounts = List(a2, a3)
    qe.save(org)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe org.accounts.map(_.toMap)

    a2.balance = 222
    qe.save(org)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe org.accounts.map(_.toMap)
    org_saved.accounts(0).balance shouldBe 222

    org.accounts = List(a2)
    qe.save(org)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe org.accounts.map(_.toMap)

    val a1k = qe.get[OrganizationAccountKeyTest]("A1").get
    a1k.organization_id = Query("organization[name = 'org'] {id}").unique[Long]
    qe.save(a1k)

    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe List(org_main_account, a2).map(_.toMap)

    val org_roc = qe.get[OrganizationReadonlyChildrenTest]("org").get
    org_roc.accounts = List(a2)
    qe.save(org_roc)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.accounts.map(_.toMap) shouldBe List(org_main_account, a2).map(_.toMap)

    val org_rfo = qe.get[OrganizationRefOnlyUpdateTest1]("org").get
    org_rfo.main_account.number   shouldBe org_main_account.number
    org_rfo.main_account.balance  shouldBe org_main_account.balance

    org_rfo.main_account.number   = a2.number
    org_rfo.main_account.balance  = a2.balance - 100
    qe.save(org_rfo)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.main_account.number   shouldBe a2.number
    org_saved.main_account.balance  shouldBe a2.balance

    org_rfo.main_account = null
    qe.save(org_rfo)
    org_saved = qe.get[OrganizationKeyTest]("org").get
    org_saved.main_account shouldBe null
  }

  if (isDbAvailable) it should s"support ambiguous ref resolvers $dbName" in {
    val m = new Mother
    var m_saved: Mother = null

    m.name = "Mommy"
    m.id = qe.save(m)

    m_saved = qe.get[Mother](m.id).get
    m_saved.name shouldBe "Mommy"
    m_saved.sex shouldBe "F"
    m_saved.daughters shouldBe Nil

    val d1 = new MotherDaughters()
    d1.name = "D1"
    val d2 = new MotherDaughters()
    d2.name = "D2"
    m.daughters = List(d1, d2)
    qe.save(m)
    m_saved = qe.get[Mother](m.id).get
    m_saved.daughters.length shouldBe 2
    m_saved.daughters(0).name shouldBe d1.name
    m_saved.daughters(0).sex  shouldBe "F"
    m_saved.daughters(1).name shouldBe d2.name
    m_saved.daughters(1).sex  shouldBe "F"
  }
}

object QuereaseDbTests {
  val conf = ConfigFactory.load()
  val MainDb:  String   = null // "querease"
  val ExtraDb: String   = "querease2"
  val dataPath = "test/data"
  class ThreadLocalDateFormat(val pattern: String) extends ThreadLocal[SimpleDateFormat] {
    override def initialValue = { val f = new SimpleDateFormat(pattern); f.setLenient(false); f }
    def apply(date: Date) = get.format(date)
    def format(date: Date) = get.format(date)
  }
  val Timestamp = new ThreadLocalDateFormat("yyyy.MM.dd HH:mm:ss.SSS")

  def loadJdbcDrivers: Unit = {
    // Load drivers sequentially to avoid deadlocks
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    Class.forName("org.postgresql.Driver")
  }

  val TresqlLogger: Logging#TresqlLogger = { (msg, _, topic) =>
    val topicName = topic match {
      case LogTopic.info   => "info  "
      case LogTopic.params => "params"
      case LogTopic.sql    => "sql --"
      case LogTopic.tresql => "tresql"
      case LogTopic.sql_with_params => null
    }
    if (topicName != null && conf.getBoolean("debug"))
      println(Timestamp(new Date()) + s"  [$topicName]  $msg")
  }

  val Env  = new ThreadLocalResources { override def logger = TresqlLogger }
  val Env2 = new ThreadLocalResources { override def logger = TresqlLogger }
  def getEnv(db: String) = db match {
    case MainDb  => Env
    case ExtraDb => Env2
  }
  def setEnv(db: String, dialect: CoreTypes.Dialect, conn: Connection) = {
    Env.extraResources = Map(ExtraDb -> Env2)
    val env = getEnv(db)
    conn.setAutoCommit(false)
    env.dialect = dialect
    env.metadata = db match {
      case MainDb   => new TresqlMetadata(qe.tableMetadata.tableDefs)
      case ExtraDb  => new TresqlMetadata(qe.tableMetadata.tableDefs).extraDbToMetadata(ExtraDb)
    }
    env.idExpr = s => "nextval('seq')"
    env.setMacros(new QuereaseTestMacros)
    env.conn = conn
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    if (Env2.conn != null) Env2.conn.close
    Env.conn = null
    Env2.conn = null
  }
  def executeStatements(db: String, statements: String*): Unit = {
    val conn = getEnv(db).conn
    val statement = conn.createStatement
    try statements foreach { s =>
      try {
        statement.execute(s)
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Failed to execute sql statement:\n$s", ex)
      }
    }
    finally statement.close()
  }
  def commit = {
    executeStatements(MainDb,  "commit")
    executeStatements(ExtraDb, "commit")
  }

  def loadSampleData: Unit = {
    loadPersonData
    loadCarData
    loadCurrencyData
  }

  implicit val resources = Env
  def loadPersonData: Unit = {
    def person(id: Long, name: String, surname: String, mId: Option[Long], fId: Option[Long]) = {
      def toId(id: Long) = java.lang.Long.valueOf(id + 1000)
      val p = new Person
      p.id = toId(id)
      p.name = name
      p.surname = surname
      p.sex = if (name endsWith "s") "M" else "F"
      p.mother_id = mId.map(toId).orNull
      p.father_id = fId.map(toId).orNull
      p
    }
    val personsString = fileToString(dataPath + "/" + "persons-in.txt")
    def savePerson(p: Person) = {
      val p2 = new Person2
      p2.id = p.id
      p2.full_name = List(p.name, p.surname).mkString(" ")
      qe.save(p,  forceInsert = true)
      qe.save(p2, forceInsert = true)
    }
    personsString split "\\n" foreach (_.trim.split("\\s+").toList match {
      case id :: name :: surname :: Nil =>
        savePerson(person(id.toLong, name, surname, None, None))
      case id :: name :: surname :: mId :: Nil =>
        savePerson(person(id.toLong, name, surname, Some(mId.toLong), None))
      case id :: name :: surname :: mId :: fId :: Nil =>
        savePerson(person(id.toLong, name, surname, Some(mId.toLong), Some(fId.toLong)))
      case Nil =>
      case x => sys.error("unexpected format: " + x)
    })
  }

  def loadCarData: Unit = {
    // sample data
    tresql"+car_schema.person_car {id, person_id, car_name} [1, 1127, 'Prius']"
    tresql"+car_schema.person_car {id, person_id, car_name} [2, 1127, 'Tesla']"
  }

  def loadCurrencyData: Unit = {
    // sample data
    val currency = new Currency
    currency.code = "EUR"
    currency.name = "Euro"
    qe.save(currency, forceInsert = true)
  }
}
