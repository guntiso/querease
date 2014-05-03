import java.sql.Connection
import java.sql.DriverManager

import org.tresql.Env
import org.tresql.dialects._

import com.typesafe.config.ConfigFactory

import dto._
import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.out._
import querease._

object Sample {
    val yamlMd = YamlMd.fromResources()
    val tableMd = new Metadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefs = (new YamlViewDefLoader(tableMd, yamlMd) with TresqlJoinsParser).viewDefs
    val i18nRules = I18nRules.suffixI18n(Set("_eng", "_rus"))
    val md = new Metadata(tableMd.tableDefs, viewDefs, i18nRules)
    val qio = QuereaseIo.scalaDto(md)
    val builder = QueryStringBuilder.default(md.extendedViewDef.get)
    val qe = new Querease(qio, builder)

    println("Entities:")
    md.tableDefs.map(_.name) foreach println
    println

    println("Views:")
    md.viewDefs.map(_.name) foreach println
    println

    val conf = ConfigFactory.load
    val url = conf.getString("sample.jdbc.url")
    val usr = conf.getString("sample.db.user")
    val pwd = conf.getString("sample.db.password")
    val dialect = conf.getString("sample.db.dialect")
    val dbg = conf.getBoolean("sample.db.statement.debug")

  def getConnection = DriverManager.getConnection(url, usr, pwd)
  def setEnv(conn: Connection = getConnection) = {
    conn.setAutoCommit(false)
    Env.dialect = Sample.dialect match {
      case "oracle" => OracleDialect
      case "hsql" => HSQLDialect
      case "ansi" | "" | null => ANSISQLDialect
    }
    Env.metaData = new TresqlMetadata(md.tableDefs, null)
    Env.conn = conn
    if (dbg) Env update { (msg, level) => println(msg) }
    else Env update { (msg, level) => () }
  }
  def clearEnv = {
    if (Env.conn != null) Env.conn.close
    Env.conn = null
  }

  def main(args: Array[String]) {
    setEnv()
    try {
      val banks = qe.list(classOf[BankListRow], null, 0, 10)
      println("Bank codes:")
      banks.map(_.code) foreach println
      println
    } finally {
      clearEnv
    }
  }
}
