package test

import java.sql.DriverManager
import org.mojoz.metadata.out.DdlGenerator
import org.tresql.dialects.PostgresqlDialect
import QuereaseDbTests.{executeStatements, setEnv, MainDb, ExtraDb}
import QuereaseTests.qe
import org.tresql.TresqlException

class QuereasePostgresTests extends QuereaseDbTests {
  import QuereasePostgresTests._
  override def dbName = "postgresql"
  override def isDbAvailable: Boolean = hasPostgres
  override def setEnv(db: String): Unit = setPostgresEnv(db)
  override def createDbObjects(db: String) = createPostgresObjects(db)
  override def interceptedSqlExceptionMessage[B](b: => B): String  =
    try {
      executeStatements(MainDb, "commit")
      executeStatements(MainDb, "begin")
      b
      throw new RuntimeException("Expected message not thrown")
    } catch {
      case ex: TresqlException =>
        executeStatements(MainDb, "rollback")
        val msg = ex.getCause.getMessage
        if (msg.startsWith("ERROR: "))
          msg.split("\n")(0).substring(7)
        else msg
    }
}

object QuereasePostgresTests {
  QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
  val conf = QuereaseDbTests.conf
  val hasPostgres = conf.getBoolean("querease.postgresql.available")
  def getPostgresConnection(db: String) = {
    val urlKeySuffix = db match {
      case MainDb   => "jdbc.url"
      case ExtraDb  => "jdbc.url2"
    }
    val url      = conf.getString("querease.postgresql." + urlKeySuffix)
    val user     = conf.getString("querease.postgresql.user")
    val password = conf.getString("querease.postgresql.password")
    val conn     = DriverManager.getConnection(url, user, password)
    conn
  }
  def setPostgresEnv(db: String) = setEnv(db, PostgresqlDialect, getPostgresConnection(db))
  val postgres_custom_functions_statements = Seq(
    """create or replace function checked_resolve(resolvable text, resolved bigint[], error_message text)
          returns bigint as $$
       begin
         if array_length(resolved, 1) > 1 or resolvable is not null and resolved[1] is null then
           raise exception sqlstate '235BX' using message = error_message;
         else
           return resolved[1];
         end if;
       end;
       $$ language plpgsql immutable""",
    """create or replace function checked_resolve(resolvable text, resolved text[], error_message text)
         returns text as $$
       begin
         if array_length(resolved, 1) > 1 or resolvable is not null and resolved[1] is null then
           raise exception sqlstate '235BX' using message = error_message;
         else
           return resolved[1];
         end if;
       end;
       $$ language plpgsql immutable"""
  )
  def createPostgresObjectsStatements(db: String) =
    Seq(
      "drop schema if exists querease cascade",
      "drop schema if exists car_schema cascade",
      "create schema querease authorization querease",
      "create schema car_schema",
    ) ++
    postgres_custom_functions_statements ++
    DdlGenerator.postgresql().schema(qe.tableMetadata.tableDefs.filter(_.db == db)).split(";").toList.map(_.trim).filter(_ != "") ++
    Seq(
      "CREATE SEQUENCE seq START WITH 10000",
      "commit",
    )
  def createPostgresObjects(db: String) = executeStatements(db, createPostgresObjectsStatements(db) : _*)
}
