import java.sql.Connection
import java.sql.DriverManager

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.tresql.Env
import org.tresql.dialects.HSQLDialect

import dto.BankListRow
import mojoz.metadata.Metadata
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
  val tableMd = new Metadata(tableDefs)
  val viewDefs = (new YamlViewDefLoader(tableMd, mdDefs) with TresqlJoinsParser).viewDefs
  val i18nRules = I18nRules.suffixI18n(Set("_eng", "_rus"))
  val md = new Metadata(tableMd.tableDefs, viewDefs, i18nRules)
  val qio = QuereaseIo.scalaDto(md)
  val builder = QueryStringBuilder.default(md.extendedViewDef.get)
  val qe = new Querease(qio, builder)
  val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")

  "querease" should "do something" in {
    def executeStatements(statements: String*) {
      val conn = getConnection
      try {
        val statement = conn.createStatement
        try statements foreach { statement.execute } finally statement.close()
      } finally conn.close()
    }
    val statements = SqlWriter.hsqldb().createStatements(tableDefs)
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
      qe.delete(b1)
      val banksAfter = qe.list(classOf[BankListRow], null)
      banksAfter.size should be(1)
      val name2 = "Bank 2 updated name"
      b2.name = name2
      qe.save(b2)
      qe.get(classOf[BankListRow], 10001).get.name should be(name2)
    } finally clearEnv
  }
  def getConnection = DriverManager.getConnection(url, user, password)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = HSQLDialect
    Env update { (msg, level) => println(msg) }
    Env.metaData = new TresqlMetadata(md.tableDefs, null)
    Env.idExpr = s => "nextval('seq')"
    Env.conn = conn
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    Env.conn = null
  }
}
