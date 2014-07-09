import java.io.PrintWriter
import java.sql.Connection
import java.sql.DriverManager

import scala.io.Source

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.tresql.Env
import org.tresql.dialects.HSQLDialect

import dto._
import mojoz.metadata.TableMetadata
import mojoz.metadata.in.I18nRules
import mojoz.metadata.in.YamlMd
import mojoz.metadata.in.YamlTableDefLoader
import mojoz.metadata.in.YamlViewDefLoader
import mojoz.metadata.out.SqlWriter
import querease.Querease
import querease.QuereaseIo
import querease.QueryStringBuilder
import querease.TresqlJoinsParser
import querease.TresqlMetadata

class QuereaseTests extends FlatSpec with Matchers {
  val path = "sample/md"
  val mdDefs = YamlMd.fromFiles(path = path)
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val i18nRules = I18nRules.suffixI18n(tableMd, Set("_eng", "_rus"))
  val viewDefs = YamlViewDefLoader(tableMd, mdDefs, TresqlJoinsParser,
    extendedViewDefTransformer = i18nRules.setI18n)
  val qio = QuereaseIo.scalaDto(viewDefs.extendedViewDefs)
  val builder = QueryStringBuilder.default(viewDefs.extendedViewDefs.get, tableMd)
  val qe = new Querease(qio, builder)
  val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  import QuereaseTests._

  "querease" should "do something" in {
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
          })
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
        p
      }
      val producedAlt = qe.list(classOf[PersonInfoAlt], null)
        .map(altToPersonInfo).map(personInfoString)
        .mkString("", "\n", "\n")
      if (expected != producedAlt)
        toFile(dataPath + "/" + "persons-out-produced-alt.txt", producedAlt)
      expected should be(producedAlt)
    } finally clearEnv
  }
  def getConnection = DriverManager.getConnection(url, user, password)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = HSQLDialect
    Env update { (msg, level) => println(msg) }
    Env.metaData = new TresqlMetadata(tableDefs, null)
    Env.idExpr = s => "nextval('seq')"
    Env.conn = conn
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    Env.conn = null
  }
}

object QuereaseTests {
  val nl = System.getProperty("line.separator")
  val dataPath = "test/data"
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
