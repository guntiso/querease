import java.sql.DriverManager

import org.tresql.Env

import com.typesafe.config.ConfigFactory

import dto._
import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.out._
import querease._

object Sample {
  def main(args: Array[String]) {
    val yamlMd = YamlMd.fromResources()
    val tableMd = new Metadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefs = (new YamlViewDefLoader(tableMd, yamlMd) with TresqlJoinsParser).viewDefs
    val i18nRules = I18nRules.suffixI18n(Set("_eng", "_rus"))
    val md = new Metadata(tableMd.tableDefs, viewDefs, i18nRules)
    val qe = new ScalaDtoQuerease(md) with Querease

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
    val dbg = conf.getBoolean("sample.db.statement.debug")
    val conn = DriverManager.getConnection(url, usr, pwd)
    conn.setAutoCommit(false)
    Env.metaData = new TresqlMetadata(md.tableDefs, null)
    Env.conn = conn
    if (dbg) Env update { (msg, level) => println(msg) }
    else Env update { (msg, level) => () }

    try {
      val banks = qe.query(classOf[BankListRow], null)
      println("Bank codes:")
      banks.map(_.code) foreach println
      println
    } finally {
      Env.conn = null
      conn.close
    }
  }
}
