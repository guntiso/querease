package test

import java.io.PrintWriter
import java.sql.Connection
import java.sql.DriverManager

import scala.io.Source

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.tresql.Env
import org.tresql.dialects.HSQLDialect

import dto._
import mojoz.metadata._
import mojoz.metadata.TableMetadata
import mojoz.metadata.in.I18nRules
import mojoz.metadata.in.YamlMd
import mojoz.metadata.in.YamlTableDefLoader
import mojoz.metadata.in.YamlViewDefLoader
import mojoz.metadata.out.SqlWriter
import querease.Querease
import querease.TresqlMetadata
import querease.ScalaDtoQuereaseIo
import querease.TresqlJoinsParser

class QuereaseTests extends FlatSpec with Matchers {
  import QuereaseTests._

  implicit val resources = Env

  "querease" should "do something" in {
    executeStatements(statements: _*)
    executeStatements("CREATE SEQUENCE seq START WITH 10000")
    println
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
      banks.map(b => (b.id, b.code, b.name)) foreach println
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
        p.motherId = mId.map(toId).orNull
        p.fatherId = fId.map(toId).orNull
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
        import p._
        def sxName(m: String, f: String) = if (sex == "M") m else f
        def hasInfo(info: String*) = info.filter(_ != null).size > 0
        List(
          List(name, surname).filter(_ != null).mkString(" "),
          if (!hasInfo(fatherName, motherName)) null else
            List(
              Option(fatherName).map(_ +
                (if (!hasInfo(paternalGrandfather, paternalGrandmother)) "" else
                  List(paternalGrandfather, paternalGrandmother)
                    .filter(_ != null).mkString(" (son of ", " and ", ")"))),
              Option(motherName).map(_ +
                (if (!hasInfo(maternalGrandfather, maternalGrandmother)) "" else
                  List(maternalGrandfather, maternalGrandmother)
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
          },if(father != null) s"father's full name ${father.name} ${father.surname}" else null)
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
        p.motherName = a.motherName
        p.fatherName = a.fatherName
        p.maternalGrandmother = a.maternalGrandmother
        p.maternalGrandfather = a.maternalGrandfather
        p.paternalGrandmother = a.paternalGrandmother
        p.paternalGrandfather = a.paternalGrandfather
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
    } finally clearEnv
  }
  "objects" should "produce correct save-to maps" in {
    def keys(instance: Dto) =
      instance.toSaveableMap.keys.toList.sorted.mkString("; ")
    def resolverKeys(instance: Dto) =
      instance.toSaveableMap.keys.filter(_.indexOf('=') >= 0).toList.sorted.mkString("; ")

    keys(new ResolverTestAccount1) should be(List(
      "code->",
      "code->bank_id=checked_resolve(_, array(bank[code = _]{id}@(2)), 'Failed to identify value of \"code\" (from resolver_test_account_1) - ' || _)",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccount2) should be(List(
      "code->",
      "code->bank_id=checked_resolve(_, array(bank[code = :'code->' && :some_other_variable]{id}@(2)), 'Failed to identify value of \"code\" (from resolver_test_account_2) - ' || _)",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccountSelfRef1) should be(List(
      "name->",
      "name->id=checked_resolve(_, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = _]{account.id}@(2)), 'Failed to identify value of \"name\" (from resolver_test_account_self_ref_1) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestBank1) should be("name->name='My bank'")
    resolverKeys(new ResolverTestBank2) should be("name->name=_ || ' saved'")
    keys(new ResolverTestAccountCurrency1) should be(List(
      "account->",
      "account->account_id=checked_resolve(_, array(account[billing_account = _]{id}@(2)), 'Failed to identify value of \"account\" (from resolver_test_account_currency_1) - ' || _)",
      "currency_name->",
      "currency_name->currency_code=checked_resolve(_, array(currency[name = _]{code}@(2)), 'Failed to identify value of \"currency_name\" (from resolver_test_account_currency_1) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson1) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || surname = _]{id}@(2)), 'Failed to identify value of \"father\" (from resolver_test_person_1) - ' || _)",
      "mother->mother_id=checked_resolve(_, array(person[name || surname = _]{id}@(2)), 'Failed to identify value of \"mother\" (from resolver_test_person_1) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson2) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#1)' = _]{person.id}@(2)), 'Failed to identify value of \"father\" (from resolver_test_person_2) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson3) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#4)' = _]{id}@(2)), 'Failed to identify value of \"father\" (from resolver_test_person_3) - ' || _)",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#2)' = _]{person.id}@(2)), 'Failed to identify value of \"mother\" (from resolver_test_person_3) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson4) should be(List(
      "father->father_id=2",
      "mother->mother_id=1"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson5) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#7)' = _]{id}@(2)), 'Failed to identify value of \"father\" (from resolver_test_person_5) - ' || _)",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#5)' = _]{person.id}@(2)), 'Failed to identify value of \"mother\" (from resolver_test_person_5) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson6) should be(List(
      "father->father_id=4",
      "mother->mother_id=3"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson7) should be(List(
      "mother->mother_id=checked_resolve(_, array(person;person[person.father_id]person? father[name || ' ' || surname || ' of ' || father.name || ' (#7)' = _]{person.id}@(2)), 'Failed to identify value of \"mother\" (from resolver_test_person_7) - ' || _)"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson8) should be(List(
      "mother->mother_id=checked_resolve(_, array(person p1;p1[p1.father_id]person? father[name || ' ' || surname || ' of ' || father.name || ' (#8)' = _]{p1.id}@(2)), 'Failed to identify value of \"mother\" (from resolver_test_person_8) - ' || _)"
    ).mkString("; "))
    resolverKeys(new NestedResolverTest1) should be(List(
      "mother->mother_id=checked_resolve(:'mother->', array(person[[name || ' ' || surname || ' of ' || father.name || ' (#7)' = :'mother->' & father_id = checked_resolve(:other_field, array(person p1[[:other_field = name || ' ' || surname || ' of ' || father.name || ' (#8)']]{p1.id}), 'Failed to identify value of \"other_field\" (from person_multitable_choice_resolver_implied_1) - ' || :other_field)]]{person.id}), 'Failed to identify value of \"mother\" (from nested_resolver_test_1) - ' || :'mother->')"
    ).mkString("; "))
  }
  "querease" should "select referenced fields correctly" in {
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_2"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[p2.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[id = p2.father_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("ref_expression_test_person_2b"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("ref_expression_test_person_2c"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_3"), Map.empty)._1 should be(
      "person p3 {" +
      "p3.id, " +
      "(person[p3.mother_id = id + 3] {person.name || ' ' || person.surname || ' (#2)' full_name}) mother, " +
      "(person[person.id = p3.father_id] {person.name || ' ' || person.surname || ' (#3)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_5"), Map.empty)._1 should be(
      "person p5 {" +
      "p5.id, " +
      "(person[p5.mother_id = id + 5] {person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p5.father_id] {person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_6"), Map.empty)._1 should be(
      "person p6 {" +
      "p6.id, " +
      "(person[person.id = p6.mother_id] {person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p6.father_id] {person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_7"), Map.empty)._1 should be(
      "person p7 {" +
      "(person; person[person.father_id father?] person[person.id = p7.mother_id] {person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_test_person_8"), Map.empty)._1 should be(
      "person p8 {" +
      "(person p1; p1[p1.father_id father?] person[p1.id = p8.mother_id] {p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDefs("ref_test_bank_2"), Map.empty)._1 should be(
      "bank {" +
      "bank.name, " +
      "(country[country.code = bank.country_code] {country.code || ' - ' || country.name c2_and_name}) country}"
    )
    qe.queryStringAndParams(qe.viewDefs("ref_test_bank_3"), Map.empty)._1 should be(
      "bank b3 {" +
      "b3.name, " +
      "(country c3[c3.code = b3.country_code] {c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )

    // test alias clash resolvement in implicit resolver joins
    qe.queryStringAndParams(qe.viewDefs("resolver_alias_clash_test_person_7_a"), Map.empty)._1 should be(
      "person {" +
      "(person_2(# mother_id) {{person.mother_id}} person_2 [person.id = person_2.mother_id] person;" +
      " person[person.father_id father?] person {person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_alias_clash_test_person_8_a"), Map.empty)._1 should be(
      "person p1 {" +
      "(p1_2(# mother_id) {{p1.mother_id}} p1_2 [p1.id = p1_2.mother_id] person p1;" +
      " p1[p1.father_id father?] person {p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDefs("resolver_alias_clash_test_person_8_b"), Map.empty)._1 should be(
      "person father {" +
      "(father_2(# mother_id) {{father.mother_id}} father_2 [p1.id = father_2.mother_id] person p1;" +
      " p1[p1.father_id father?] person {p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )

    // test implied join to self
    qe.queryStringAndParams(qe.viewDefs("self_ref_test_account_1"), Map.empty)._1 should be(
      "account {(account_2(# id) {{account.id}} account_2 [account.id = account_2.id] account;" +
      " account/bank? {bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDefs("self_ref_test_account_2"), Map.empty)._1 should be(
      "account a1 {(account; account/bank?[account.id = a1.id] {bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )

    // test ambiguity resolver
    qe.queryStringAndParams(qe.viewDefs("ambiguity_resolver_test_person_1"), Map.empty)._1 should be(
      "person p1 {" +
      "(person[person.id = p1.id] {person.name || ' ' || person.surname || ' (#1)' full_name}) this_name, " +
      "(person[person.id = p1.father_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) fath_name, " +
      "(person[person.id = p1.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) moth_name, " +
      "(person[person.id = p1.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p1.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) ma}"
    )
  }
}

object QuereaseTests {
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

  val qe = new Querease with ScalaDtoQuereaseIo {

    override type DTO = Dto

    private val i18nRules = I18nRules.suffixI18n(tableMetadata, Set("_eng", "_rus"))
    override lazy val tableMetadata =
      new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions).tableDefs, dbName)
    override lazy val yamlMetadata = YamlMd.fromFiles(path = "sample/md")
    override lazy val viewDefs = YamlViewDefLoader(
      tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions)
        .extendedViewDefs.mapValues(i18nRules.setI18n(_).asInstanceOf[ViewDef])
    override def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
      Naming.dasherize(mf.runtimeClass.getSimpleName).replace("-", "_")
  }
  val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  val nl = System.getProperty("line.separator")
  val dataPath = "test/data"
  def getConnection = DriverManager.getConnection(url, user, password)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = HSQLDialect
    Env.logger = (msg, level) => println(msg)
    Env.metadata = new TresqlMetadata(qe.tableMetadata.tableDefs, null)
    Env.idExpr = s => "nextval('seq')"
    Env.conn = conn
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    Env.conn = null
  }
  Class.forName("org.hsqldb.jdbc.JDBCDriver") // fix "sbt +test" - No suitable driver found
  def executeStatements(statements: String*) {
    val conn = getConnection
    try {
      val statement = conn.createStatement
      try statements foreach { statement.execute } finally statement.close()
    } finally conn.close()
  }
  val statements = SqlWriter.hsqldb().schema(qe.tableMetadata.tableDefs)
    .split(";").toList.map(_.trim).filter(_ != "")
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body.replace(nl, "\n") // normalize newlines
  }
  def toFile(filename: String, message: String) {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}
