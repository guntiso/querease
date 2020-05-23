package test

import java.io.PrintWriter
import java.sql.{Connection, DriverManager, Timestamp}
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.tresql._
import org.tresql.dialects.HSQLDialect
import dto._
import mojoz.metadata._
import mojoz.metadata.TableMetadata
import mojoz.metadata.in.I18nRules
import mojoz.metadata.in.YamlMd
import mojoz.metadata.in.YamlTableDefLoader
import mojoz.metadata.in.YamlViewDefLoader
import mojoz.metadata.out.SqlWriter
import querease._


class QuereaseTests extends FlatSpec with Matchers {
  import QuereaseTests._

  "querease" should "do something" in {
    executeStatements(statements: _*)
    executeStatements("CREATE SEQUENCE seq START WITH 10000")
    setEnv()
    try {
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

      def person(id: Long, name: String, surname: String, mId: Option[Long], fId: Option[Long]) = {
        def toId(id: Long) = new java.lang.Long(id + 1000)
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
      personsString split "\\n" foreach (_.trim.split("\\s+").toList match {
        case id :: name :: surname :: Nil =>
          qe.save(
            person(id.toLong, name, surname, None, None),
            forceInsert = true)
        case id :: name :: surname :: mId :: Nil =>
          qe.save(
            person(id.toLong, name, surname, Some(mId.toLong), None),
            forceInsert = true)
        case id :: name :: surname :: mId :: fId :: Nil =>
          qe.save(
            person(id.toLong, name, surname, Some(mId.toLong), Some(fId.toLong)),
            forceInsert = true)
        case Nil =>
        case x => sys.error("unexpected format: " + x)
      })
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
        toFile(dataPath + "/" + "persons-out-produced.txt", produced)
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
        toFile(dataPath + "/" + "persons-out-produced-alt.txt", producedAlt)
      expected should be(producedAlt)

      val siblingsExpected = fileToString(dataPath + "/" + "siblings-out.txt")
      val siblingsProduced =
        qe.list[Siblings](null)
          .map(s => List(s.sibling1, s.sibling2).filter(_ != null).mkString(", "))
          .mkString("", "\n", "\n")
      if (siblingsExpected != siblingsProduced)
        toFile(dataPath + "/" + "siblings-out-produced.txt", siblingsProduced)
      siblingsExpected should be(siblingsProduced)

      val siblingsProducedAlt =
        qe.list[SiblingsAlt](null)
          .map(s => List(s.sibling1, s.sibling2).filter(_ != null).mkString(", "))
          .mkString("", "\n", "\n")
      if (siblingsExpected != siblingsProducedAlt)
        toFile(dataPath + "/" + "siblings-out-produced-alt.txt", siblingsProducedAlt)
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
        toFile(dataPath + "/" + "father-tree-out-produced.txt", producedFatherTree)
      expectedFatherTree should be(producedFatherTree)

      val expectedForefathers = fileToString(dataPath + "/" + "forefathers-out.txt")
      val producedForefathers =
        qe.list[WithForefathers](null).toList.flatMap { p => List(
          p.full_name,
          s"  ${p.forefathers.map(_.full_name).mkString(", ")}"
        )}.filterNot(_.trim == "").mkString("\n")
      if (expectedForefathers != producedForefathers)
        toFile(dataPath + "/" + "forefathers-out-produced.txt", producedForefathers)
      expectedForefathers should be(producedForefathers)

      // filter resolver tests with optional bind variable
      qe.countAll[FilterWithResolverTest1](Map.empty) shouldBe 0
      qe.countAll[FilterWithResolverTest1](Map("mother" -> null)) shouldBe 0
      qe.countAll[FilterWithResolverTest1](Map("mother" -> ("Minna" + "Priedīte"))) shouldBe 1
      qe.countAll[FilterWithResolverTest1](Map("mother" -> ("Helēna" + "Stūrīte"))) shouldBe 6
      qe.list    [FilterWithResolverTest1](Map("mother" -> ("Helēna" + "Stūrīte"))).size shouldBe 6
      (intercept[java.sql.SQLException] {
        qe.countAll[FilterWithResolverTest1](Map("mother" -> "dada"))
      }).getMessage shouldBe """Failed to identify value of "mother" (from filter_with_resolver_test_1) - dada"""

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

      //resolver test with bind variable from substructure
      val acc = new AccountWithBank
      val accb = new AccountWithBankBank
      val bacNr = "123456789"
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
        Map("id" -> null, "name" -> "Some", "surname" -> "Child", "sex" -> "M", "mother" -> null, "father" -> null)
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
      (intercept[java.sql.SQLException] {
        child.resolve_father_id
      }).getMessage shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - Some, Father"""
      (intercept[java.sql.SQLException] {
        PersonWithComplexTypeResolvers1.resolve_father_id(child.father)
      }).getMessage shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - Some, Father"""
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
      (intercept[java.sql.SQLException] {
        person9.resolve_mother_id(surname = "Mother", `type` = "crocodile")
      }).getMessage shouldBe """Failed to identify value of "mother_id" (from resolver_test_person_9_a) - Some, Mother, crocodile"""
      person9.resolve_mother_id(surname = "Mother", `type` = "person") shouldBe motherId
      (intercept[java.sql.SQLException] {
        ResolverTestPerson9A.resolve_mother_id(name = "Some", surname = "Mother", `type` = "crocodile")
      }).getMessage shouldBe """Failed to identify value of "mother_id" (from resolver_test_person_9_a) - Some, Mother, crocodile"""
      ResolverTestPerson9A.resolve_mother_id("Some", "Mother", "person") shouldBe motherId
      ResolverTestPerson9A.resolve_mother_id(name = "Some", surname = "Mother", `type` = "person") shouldBe motherId
      ResolverTestPerson9B.resolve_mother_id(name = "Some", surname = "Mother", `creative param name` = "person") shouldBe motherId
      ResolverTestPerson9C.resolve_mother_id(name = "Some", surname = "Mother", `creative.param.name` = "person") shouldBe motherId

      qe.countAll[ResolverTestPerson10](Map("name" -> "Alfrēds")) shouldBe 1
      qe.countAll[ResolverTestPerson10](Map("name" -> "Marija" )) shouldBe 2
      ResolverTestPerson10.resolve_id("Alfrēds") shouldBe 1108
      (intercept[java.sql.SQLException] {
        ResolverTestPerson10.resolve_id(name = "Marija")
      }).getMessage shouldBe """Failed to identify value of "id" (from resolver_test_person_10) - Marija"""
      (intercept[java.sql.SQLException] {
        ResolverTestPerson11.resolve_id(name = "Alfrēds")
      }).getMessage shouldBe """Failed to identify value of "id" (from resolver_test_person_11) - Alfrēds"""
      ResolverTestPerson12A.resolve_father_id("Alfrēds", 1) shouldBe 1108
      ResolverTestPerson12B.resolve_father_id("Alfrēds", 1) shouldBe 1108
      (intercept[java.sql.SQLException] {
        ResolverTestPerson12B.resolve_father_id("Alfrēds", 0)
      }).getMessage shouldBe """Failed to identify value of "father" (from resolver_test_person_12_b) - Alfrēds"""
      ResolverTestPerson12C.resolve_father_id("Alfrēds", 1) shouldBe 1108
      (intercept[java.sql.SQLException] {
        ResolverTestPerson12C.resolve_father_id("Alfrēds", 0)
      }).getMessage shouldBe """Failed to identify value of "father" (from resolver_test_person_12_c) - Alfrēds, 0"""

      var child2 = qe.get[PersonWithComplexTypeResolvers2](childId).get
      child2.resolve_father_id(m => m + ("father" -> (m("father").asInstanceOf[Map[String, Any]] ++ Map("is_resolver_disabled" -> false)))) shouldBe fatherId
      (intercept[java.sql.SQLException] {
        child2.resolve_father_id(m => m + ("father" -> (m("father").asInstanceOf[Map[String, Any]] ++ Map("is_resolver_disabled" -> true))))
      }).getMessage shouldBe """Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - Some, Father, true"""
      child2.resolve_father_id(m => m) shouldBe fatherId

      // sample data
      val currency = new Currency
      currency.code = "EUR"
      currency.name = "Euro"
      qe.save(currency)

      val accountCurrency = new AccountCurrency
      accountCurrency.account_id = accountId
      accountCurrency.currency_code = "EUR"
      qe.save(accountCurrency)

      // dto resolver tests
      PersonChoiceResolverImplied.resolve_id("Guntis Ozols (#2)") shouldBe 1127
      (intercept[java.sql.SQLException] {
        PersonChoiceResolverImplied.resolve_id("blah blah")
      }).getMessage shouldBe """Failed to identify value of "full_name" (from person_choice_resolver_implied) - blah blah"""
      val personChoiceResolverImplied = new PersonChoiceResolverImplied
      personChoiceResolverImplied.full_name = "Andris Ozols (#2)"
      personChoiceResolverImplied.resolve_id shouldBe 1128
      (intercept[java.sql.SQLException] {
        personChoiceResolverImplied.full_name = "dada dada"
        personChoiceResolverImplied.resolve_id
      }).getMessage shouldBe """Failed to identify value of "full_name" (from person_choice_resolver_implied) - dada dada"""
      ResolverTestAccountCurrency1.resolve_account_id(bacNr) shouldBe accountId
      (intercept[java.sql.SQLException] {
        ResolverTestAccountCurrency1.resolve_account_id(s"$bacNr-X")
      }).getMessage shouldBe """Failed to identify value of "account" (from resolver_test_account_currency_1) - 123456789-X"""
      ResolverTestAccountCurrency1.resolve_currency_code("Euro") shouldBe "EUR"
      (intercept[java.sql.SQLException] {
        ResolverTestAccountCurrency1.resolve_currency_code("Euro-X")
      }).getMessage shouldBe """Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - Euro-X"""
      val resolverTestAccountCurrency1 = new ResolverTestAccountCurrency1
      resolverTestAccountCurrency1.account = bacNr
      resolverTestAccountCurrency1.currency_name = "Euro"
      resolverTestAccountCurrency1.resolve_account_id shouldBe accountId
      resolverTestAccountCurrency1.resolve_currency_code shouldBe "EUR"
      resolverTestAccountCurrency1.account += "-Z"
      resolverTestAccountCurrency1.currency_name += "-Z"
      (intercept[java.sql.SQLException] {
        resolverTestAccountCurrency1.resolve_account_id
      }).getMessage shouldBe """Failed to identify value of "account" (from resolver_test_account_currency_1) - 123456789-Z"""
      (intercept[java.sql.SQLException] {
        resolverTestAccountCurrency1.resolve_currency_code
      }).getMessage shouldBe """Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - Euro-Z"""
    } finally clearEnv
  }
  "objects" should "produce correct save-to maps" in {
    def asKeys(instance: Dto) =
      qe.toSaveableMap(instance).toList.sortBy(_._1).map {
        case (k, v) if k endsWith "->" => k + v
        case (k, v) => k
      }
    def keys(instance: Dto) =
      asKeys(instance).mkString("; ")
    def resolverKeys(instance: Dto) =
      asKeys(instance).filter(_.indexOf("->") >= 0).toList.sorted.mkString("; ")

    keys(new ResolverTestAccount1) should be(List(
      "code",
      "code->bank_id=checked_resolve(_, array(bank[code = _]{id}@(2))," +
          " 'Failed to identify value of \"code\" (from resolver_test_account_1) - ' || coalesce(_, 'null'))",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccount2) should be(List(
      "code",
      "code->bank_id=checked_resolve(coalesce(:code, :some_other_variable), array(bank[code = :code && :some_other_variable]{id}@(2))," +
          " 'Failed to identify value of \"code\" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable, 'null')))",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccountSelfRef1) should be(List(
      "name",
      "name->id=checked_resolve(_, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = _]{account.id}@(2))," +
          " 'Failed to identify value of \"name\" (from resolver_test_account_self_ref_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestBank1) should be("name->name='My bank'")
    resolverKeys(new ResolverTestBank2) should be("name->name=_ || ' saved'")
    keys(new ResolverTestAccountCurrency1) should be(List(
      "account",
      "account->account_id=checked_resolve(_, array(account[billing_account = _]{id}@(2))," +
          " 'Failed to identify value of \"account\" (from resolver_test_account_currency_1) - ' || coalesce(_, 'null'))",
      "currency_name",
      "currency_name->currency_code=checked_resolve(_, array(currency[name = _]{code}@(2))," +
          " 'Failed to identify value of \"currency_name\" (from resolver_test_account_currency_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson1) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || surname = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_1) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || surname = _]{id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson2) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#1)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_2) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson3) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#4)' = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_3) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#2)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_3) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson4) should be(List(
      "father->father_id=2",
      "mother->mother_id=1"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson5) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#7)' = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_5) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#5)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_5) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson6) should be(List(
      "father->father_id=4",
      "mother->mother_id=3"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson7) should be(List(
      "mother->mother_id=checked_resolve(_, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_7) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson8) should be(List(
      "mother->mother_id=checked_resolve(_, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = _]{p1.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_8) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new NestedResolverTest1) should be(List(
      "mother->mother_id=checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother &" +
        " person.father_id = checked_resolve(:other_field, array(" +
          "person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2))," +
          " 'Failed to identify value of \"other_field\" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field, 'null'))]]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))"
    ).mkString("; "))
  }
  "querease" should "select referenced fields correctly" in {
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_2"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_expression_test_person_2b"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_expression_test_person_2c"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_3"), Map.empty)._1 should be(
      "person p3 {" +
      "p3.id, " +
      "(person[p3.mother_id = id + 3]{person.name || ' ' || person.surname || ' (#2)' full_name}) mother, " +
      "(person[person.id = p3.father_id]{person.name || ' ' || person.surname || ' (#3)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_5"), Map.empty)._1 should be(
      "person p5 {" +
      "p5.id, " +
      "(person[p5.mother_id = id + 5]{person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p5.father_id]{person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_6"), Map.empty)._1 should be(
      "person p6 {" +
      "p6.id, " +
      "(person[person.id = p6.mother_id]{person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p6.father_id]{person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_7"), Map.empty)._1 should be(
      "person p7 {" +
      "(person;person[person.father_id]person? father[person.id = p7.mother_id]{person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_8"), Map.empty)._1 should be(
      "person p8 {" +
      "(person p1;p1[p1.father_id]person? father[p1.id = p8.mother_id]{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_2"), Map.empty)._1 should be(
      "bank {" +
      "bank.name, " +
      "(country[country.code = bank.country_code]{country.code || ' - ' || country.name c2_and_name}) country}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_3"), Map.empty)._1 should be(
      "bank b3 {" +
      "b3.name, " +
      "(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )

    // test alias clash resolvement in implicit resolver joins
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_7_a"), Map.empty)._1 should be(
      "person {" +
      "(person_2 (# mother_id) { null{person.mother_id} } person_2[person.id = person_2.mother_id]person;" +
      "person[person.father_id]person? father{person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_8_a"), Map.empty)._1 should be(
      "person p1 {" +
      "(p1_2 (# mother_id) { null{p1.mother_id} } p1_2[p1.id = p1_2.mother_id]person p1;" +
      "p1[p1.father_id]person? father{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_8_b"), Map.empty)._1 should be(
      "person father {" +
      "(father_2 (# mother_id) { null{father.mother_id} } father_2[p1.id = father_2.mother_id]person p1;" +
      "p1[p1.father_id]person? father{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )

    // test implied join to self
    qe.queryStringAndParams(qe.viewDef("self_ref_test_account_1"), Map.empty)._1 should be(
      "account {(account_2 (# id) { null{account.id} } account_2[account.id = account_2.id]account;" +
      "account/bank?{bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("self_ref_test_account_2"), Map.empty)._1 should be(
      "account a1 {(account;account/bank?[account.id = a1.id]{bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )

    // test ambiguity resolver
    qe.queryStringAndParams(qe.viewDef("ambiguity_resolver_test_person_1"), Map.empty)._1 should be(
      "person p1 {" +
      "(person[person.id = p1.id]{person.name || ' ' || person.surname || ' (#1)' full_name}) this_name, " +
      "(person[person.id = p1.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) fath_name, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) moth_name, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) ma}"
    )

    // test resolver in filter
    qe.queryStringAndParams(qe.viewDef("filter_with_resolver_test_1"), Map("mother" -> "mother"))._1 should be(
      "person" +
      "[mother_id = checked_resolve(if_defined_or_else(:mother?, :mother?, null)," +
      " array(person;person[person.mother_id]person? mother[" +
      "[if_defined_or_else(:mother?, mother.name || mother.surname = :mother & person.id ~ '%6', false)]]{person.mother_id}@(2))," +
      " 'Failed to identify value of \"mother\" (from filter_with_resolver_test_1) - ' ||" +
      " if_defined_or_else(:mother?, coalesce(:mother?, 'null'), '[missing]'))" +
      "] {person.name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_resolver_test_2"), Map("mother" -> "mother"))._1 should be(
      "person" +
      "[person[mother_id = checked_resolve(if_defined_or_else(:mother?, :mother?, null)," +
      " array(person[[person.name || ' ' || person.surname || ' (#1)' = :mother?]]{person.id}@(2))," +
      " 'Failed to identify value of \"mother\" (from filter_with_resolver_test_2) - ' ||" +
      " if_defined_or_else(:mother?, coalesce(:mother?, 'null'), '[missing]'))]{1}" +
      "] {person.name}"
    )

    // test field-ref in filter
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_4"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :country_c3_and_name?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_5"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :country_choice_3_c3_and_name?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_6"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :cc?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_1"), Map("full_name" -> "full_name"))._1 should be(
      "person[person.name || ' ' || person.surname = :full_name?] {person.name || ' ' || person.surname full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_2"), Map("full_name" -> "full_name"))._1 should be(
      "person[person.name || ' ' || person.surname = :full_name] {person.name || ' ' || person.surname full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_3"), Map("full_name" -> "full_name"))._1 should be(
      "person[(person_2 (# id) { null{person.id} } person_2[person.id = person_2.id]person{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person_2 (# id) { null{person.id} } person_2[person.id = person_2.id]person{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_4"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_5"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.father_id]{person.name || ' ' || person.surname full_name}) father_full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_6"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.father_id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_7"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
  }
  "implicit querease" should "be found" in {
    val c1 = new dto.PersonName
    c1.name = "Simona"
    val c2 = new dto.PersonName
    c2.name = "Sigita"
    val pi = new dto.PersonInfo
    pi.name = "Gunzi"
    pi.father_name = "Juris"
    pi.sex = "V"
    pi.children = List(c1, c2);
    pi.toString should be(
      "dto.PersonInfo{name: Gunzi, surname: null, sex: V, mother_name: null, father_name: Juris, " +
      "maternal_grandmother: null, maternal_grandfather: null, paternal_grandmother: null, paternal_grandfather: null, " +
      "children: (dto.PersonName{name: Simona}, dto.PersonName{name: Sigita}), father: null}"
    )
    pi.toMap should be(
      Map("name" -> "Gunzi", "surname" -> null, "sex" -> "V", "mother_name" -> null, "father_name" -> "Juris",
          "maternal_grandmother" -> null, "maternal_grandfather" -> null, "paternal_grandmother" -> null, "paternal_grandfather" -> null,
          "children" -> List(Map("name" -> "Simona"), Map("name" -> "Sigita")), "father" -> null))
  }

  "dto generator" should "generate correct dto code" in {
    val generator = new ScalaDtoGenerator(qe)
    val dtosPath = "test/dtos"
    val expectedDtos: String = fileToString(dtosPath + "/" + "dtos-out.scala")
    val producedDtos: String = generator.createScalaClassesString(Nil, qe.viewDefs.values.toList.sortBy(_.name), Nil)
    if (expectedDtos != producedDtos)
      toFile(dtosPath + "/" + "dtos-out-produced.scala", producedDtos)
    expectedDtos shouldBe producedDtos
  }

  "tresql metadata" should "report correct table metadata" in {
    val md = new TresqlMetadata(qe.tableMetadata.tableDefs)
    val mdPath = "test/metadata"
    val expectedMd: String = fileToString(mdPath + "/" + "tresql-table-metadata-out.yaml")
    val producedMd: String = md.tableMetadataString
    if (expectedMd != producedMd)
      toFile(mdPath + "/" + "tresql-table-metadata-out-produced.yaml", producedMd)
    expectedMd shouldBe producedMd
  }
}

object QuereaseTests {
  class ThreadLocalDateFormat(val pattern: String) extends ThreadLocal[SimpleDateFormat] {
    override def initialValue = { val f = new SimpleDateFormat(pattern); f.setLenient(false); f }
    def apply(date: Date) = get.format(date)
    def format(date: Date) = get.format(date)
  }
  val Timestamp = new ThreadLocalDateFormat("yyyy.MM.dd HH:mm:ss.SSS")

  implicit val Env = new ThreadLocalResources {
    override def logger = { (msg, _, topic) =>
      val topicName = topic match {
        case LogTopic.info   => "info  "
        case LogTopic.params => "params"
        case LogTopic.sql    => "sql --"
        case LogTopic.tresql => "tresql"
        case LogTopic.sql_with_params => null
      }
      if (topicName != null) println(Timestamp(new Date()) + s"  [$topicName]  $msg")
    }
  }
  def dbName(name: String) =
    Naming.dbName(name)
      .replace("_1", "1") // no underscore before 1 in our database names
      .replace("_2", "2") // no underscore before 2 in our database names
      .replace("_3", "3") // no underscore before 3 in our database names
      .replace("_4", "4") // no underscore before 4 in our database names
      .replace("_5", "5") // no underscore before 5 in our database names
      .replace("_6", "6") // no underscore before 6 in our database names
      .replace("_7", "7") // no underscore before 7 in our database names
      .replace("_8", "8") // no underscore before 8 in our database names
      .replace("_9", "9") // no underscore before 9 in our database names

   object TestQuerease extends Querease with ScalaDtoQuereaseIo {

     override type DTO = Dto

     private val i18nRules = I18nRules.suffixI18n(tableMetadata, Set("_eng", "_rus"))
     override lazy val tableMetadata =
       new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions).tableDefs, dbName)
     override lazy val yamlMetadata =
        YamlMd.fromFiles(path = "test/tables") ++
        YamlMd.fromFiles(path = "test/views")
     override lazy val viewDefs = YamlViewDefLoader(
       tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions)
       .extendedViewDefs.toMap
     override protected lazy val viewNameToFieldOrdering =
       viewDefs.map(kv => (kv._1, new FieldOrdering(
         kv._2.fields
          .map(f => Option(f.alias) getOrElse f.name)
          .zipWithIndex.toMap)
       ))
     override def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
       Naming.dasherize(mf.runtimeClass.getSimpleName).replace("-", "_")
   }

  implicit val qe = TestQuerease
  val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  val nl = System.getProperty("line.separator")
  val dataPath = "test/data"
  def getConnection = DriverManager.getConnection(url, user, password)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = HSQLDialect
    Env.metadata = new TresqlMetadata(qe.tableMetadata.tableDefs)
    Env.idExpr = s => "nextval('seq')"
    Env.setMacros(new Macros)
    Env.conn = conn
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    Env.conn = null
  }
  Class.forName("org.hsqldb.jdbc.JDBCDriver") // fix "sbt +test" - No suitable driver found
  def executeStatements(statements: String*): Unit = {
    val conn = getConnection
    try {
      val statement = conn.createStatement
      try statements foreach { statement.execute } finally statement.close()
    } finally conn.close()
  }
  val hsqldb_custom_functions_statements = Seq(
    """create function array_length(sql_array bigint array) returns int
       language java deterministic no sql
       external name 'CLASSPATH:test.HsqldbCustomFunctions.array_length'""",
    """create function array_length(sql_array char varying(1024) array) returns int
       language java deterministic no sql
       external name 'CLASSPATH:test.HsqldbCustomFunctions.array_length'""",
    """create function checked_resolve(
         resolvable char varying(1024), resolved bigint array, error_message char varying(1024)
       ) returns bigint
         if array_length(resolved) > 1 or resolvable is not null and (array_length(resolved) = 0 or resolved[1] is null) then
           signal sqlstate '45000' set message_text = error_message;
         elseif array_length(resolved) = 1 then
           return resolved[1];
         else
           return null;
         end if""",
    """create function checked_resolve(
         resolvable char varying(1024), resolved char varying(1024) array, error_message char varying(1024)
       ) returns  char varying(1024)
         if array_length(resolved) > 1 or resolvable is not null and (array_length(resolved) = 0 or resolved[1] is null) then
           signal sqlstate '45000' set message_text = error_message;
         elseif array_length(resolved) = 1 then
           return resolved[1];
         else
           return null;
         end if"""
  )
  val statements = SqlWriter.hsqldb().schema(qe.tableMetadata.tableDefs)
    .split(";").toList.map(_.trim).filter(_ != "") ++
    hsqldb_custom_functions_statements
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body.replace(nl, "\n") // normalize newlines
  }
  def toFile(filename: String, message: String): Unit = {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}
