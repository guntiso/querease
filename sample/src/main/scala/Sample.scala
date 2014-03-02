import java.sql.DriverManager

import org.tresql.Env

import com.typesafe.config.ConfigFactory

import dto._
import metadata._
import querease._

trait SampleI18nRules extends SuffixI18nRules { this: Metadata =>
  override val i18nSuffixes = Set("_eng", "_rus")
}

object SampleMetadata
  extends YamlTableDefLoader
  with ResourcesMdSource
  with SampleI18nRules
  with TresqlJoinsParser
  with AllExpressionsFilterable
  with YamlViewDefLoader
  with Metadata
  with YamlMdWriter
  with XsdWriter
  with Querease
  with ScalaDtoQuerease 

object Sample {
  def main(args: Array[String]) {
    val md = SampleMetadata

    println("Entities:")
    md.entities.map(_.name) foreach println
    println

    println("Views:")
    md.typedefs.map(_.name) foreach println
    println

    val conf = ConfigFactory.load
    val url = conf.getString("sample.jdbc.url")
    val usr = conf.getString("sample.db.user")
    val pwd = conf.getString("sample.db.password")
    val dbg = conf.getBoolean("sample.db.statement.debug")
    val conn = DriverManager.getConnection(url, usr, pwd)
    conn.setAutoCommit(false)
    Env.metaData = new TresqlMetadata(md.entities, null)
    Env.conn = conn
    if (dbg) Env update { (msg, level) => println(msg) }
    else Env update { (msg, level) => () }

    try {
      val banks = md.query(classOf[BankListRow], null)
      println("Bank codes:")
      banks.map(_.code) foreach println
      println
    } finally {
      Env.conn = null
      conn.close
    }
  }
}
