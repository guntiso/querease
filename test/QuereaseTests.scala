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
      qe.countAll(classOf[BankListRow], null) should be(2)
      val b1 = qe.get(classOf[BankListRow], 10000).get
      b1.name should be("Bank 1")
      val b2 = qe.get(classOf[BankListRow], 10001).get
      b2.name should be("Bank 2")
      qe.list(classOf[BankListRow], null)(0).id should be(10001)
      val banks = qe.list(classOf[BankListRow], null, orderBy = "id")
      banks.map(b => (b.id, b.code, b.name)) foreach println
      banks(0).id should be(10000)
      banks(0).name should be("Bank 1")
      banks(1).id should be(10001)
      banks(1).code should be("b2")
      banks.size should be(2)
      qe.list(classOf[BankListRowWithFilter], null).size should be(1)
      qe.delete(b1)
      val banksAfter = qe.list(classOf[BankListRow], null)
      banksAfter.size should be(1)
      val name2 = "Bank 2 updated name"
      b2.name = name2
      qe.save(b2)
      qe.get(classOf[BankListRow], 10001).get.name should be(name2)
      qe.save(bank)
      qe.save(bank)
      qe.list(classOf[BankListRow], null).size should be(3)
      qe.list(classOf[BankListRowWithGroup], null).size should be(2)
      val banksHv = qe.list(classOf[BankListRowWithHaving], null)
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
      val produced = qe.list(classOf[PersonInfo], null).map(personInfoString)
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
      val producedAlt = qe.list(classOf[PersonInfoAlt], null)
        .map(altToPersonInfo).map(personInfoString)
        .mkString("", "\n", "\n")
      if (expected != producedAlt)
        toFile(dataPath + "/" + "persons-out-produced-alt.txt", producedAlt)
      expected should be(producedAlt)

      val siblingsExpected = fileToString(dataPath + "/" + "siblings-out.txt")
      val siblingsProduced =
        qe.list(classOf[Siblings], null)
          .map(s => List(s.sibling1, s.sibling2).filter(_ != null).mkString(", "))
          .mkString("", "\n", "\n")
      if (siblingsExpected != siblingsProduced)
        toFile(dataPath + "/" + "siblings-out-produced.txt", siblingsProduced)
      siblingsExpected should be(siblingsProduced)

      val siblingsProducedAlt =
        qe.list(classOf[SiblingsAlt], null)
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
        fatherTreeList("", qe.list(classOf[FatherTree], null).toList, Nil)
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
      "code->bank_id=bank[code = _]{id}",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccount2) should be(List(
      "code->",
      "code->bank_id=bank[code = :'code->' && :'some_other_variable']{id}",
      "id"
    ).mkString("; "))
    resolverKeys(new ResolverTestBank1) should be("name->name='My bank'")
    resolverKeys(new ResolverTestBank2) should be("name->name=_ || ' saved'")
    keys(new ResolverTestAccountCurrency1) should be(List(
      "account->",
      "account->account_id=account[billing_account = _]{id}",
      "currency_name->",
      "currency_name->currency_code=currency[name = _]{code}"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson1) should be(List(
      "father->father_id=person[name || surname = _]{id}",
      "mother->mother_id=person[name || surname = _]{id}"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson2) should be(List(
      "father->father_id=person[name || ' ' || surname || ' (#1)' = _]{id}"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson3) should be(List(
      "father->father_id=person[name || ' ' || surname || ' (#4)' = _]{id}",
      "mother->mother_id=person[name || ' ' || surname || ' (#2)' = _]{id}"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson4) should be(List(
      "father->father_id=2",
      "mother->mother_id=1"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson5) should be(List(
      "father->father_id=person[name || ' ' || surname || ' (#7)' = _]{id}",
      "mother->mother_id=person[name || ' ' || surname || ' (#5)' = _]{id}"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson6) should be(List(
      "father->father_id=4",
      "mother->mother_id=3"
    ).mkString("; "))
  }
  "querease" should "select referenced fields correctly" in {
    qe.queryStringAndParams(xViewDefs("resolver_test_person_2"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[p2.mother_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[id = p2.father_id] {person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(xViewDefs("resolver_test_person_3"), Map.empty)._1 should be(
      "person p3 {" +
      "p3.id, " +
      "(person[p3.mother_id = id + 3] {person.name || ' ' || person.surname || ' (#2)' full_name}) mother, " +
      "(person[p3.father_id] {person.name || ' ' || person.surname || ' (#3)' full_name}) father}"
    )
    qe.queryStringAndParams(xViewDefs("resolver_test_person_5"), Map.empty)._1 should be(
      "person p5 {" +
      "p5.id, " +
      "(person[p5.mother_id = id + 5] {person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[p5.father_id] {person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(xViewDefs("resolver_test_person_6"), Map.empty)._1 should be(
      "person p6 {" +
      "p6.id, " +
      "(person[p6.mother_id] {person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[p6.father_id] {person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
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
  val path = "sample/md"
  val mdDefs = YamlMd.fromFiles(path = path)
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs, dbName)
  val i18nRules = I18nRules.suffixI18n(tableMd, Set("_eng", "_rus"))
  val xViewDefs = YamlViewDefLoader(tableMd, mdDefs, TresqlJoinsParser)
    .extendedViewDefs
    .mapValues(i18nRules.setI18n)
  val qe = new Querease with ScalaDtoQuereaseIo {
    type FieldDef = mojoz.metadata.FieldDef.FieldDefBase[Type]
    type ViewDef = mojoz.metadata.ViewDef.ViewDefBase[FieldDef]
    private lazy val viewNameToFieldOrdering =
      xViewDefs.map(kv => (kv._1, FieldOrdering(kv._2)))
    override def nameToExtendedViewDef = xViewDefs
    override def tableMetadata = tableMd
    override def viewDefOption(viewName: String): Option[ViewDef] =
      xViewDefs.get(viewName)
    override def fieldOrdering(viewName: String): Ordering[String] =
      viewNameToFieldOrdering.get(viewName) getOrElse Ordering[String]
  }
  val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  val nl = System.getProperty("line.separator")
  val dataPath = "test/data"
  def getConnection = DriverManager.getConnection(url, user, password)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = HSQLDialect
    Env.logger = (msg, level) => println(msg)
    Env.metaData = new TresqlMetadata(tableDefs, null)
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
  val statements = SqlWriter.hsqldb().schema(tableDefs)
    .split(";").toList.map(_.trim).filter(_ != "")
  def tresql(viewName: String, params: Map[String, Any] = Map.empty): String =
    tresql(xViewDefs(viewName), params)
  def tresql(view: ViewDef[FieldDef[Type]], params: Map[String, Any]): String =
    qe.queryStringAndParams(view, params)._1
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
