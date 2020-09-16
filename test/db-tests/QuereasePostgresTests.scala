package test

import java.sql.DriverManager
import com.typesafe.config.ConfigFactory

import java.sql.DriverManager
import org.mojoz.metadata.out.SqlWriter
import org.tresql.dialects.PostgresqlDialect
import QuereaseDbTests.{setEnv, executeStatements}
import QuereaseTests.qe

class QuereasePostgresTests extends QuereaseDbTests {
  import QuereasePostgresTests._
  override def dbName = "postgresql"
  override def isDbAvailable: Boolean = hasPostgres
  override def setEnv: Unit = setPostgresEnv
  override def createDbObjects = executeStatements(createPostgresObjectsStatements: _*)
  override def interceptedSqlExceptionMessage[B](b: => B): String  =
    try {
      executeStatements("commit")
      executeStatements("begin")
      b
      throw new RuntimeException("Expected message not thrown")
    } catch {
      case ex: java.sql.SQLException =>
        executeStatements("rollback")
        val msg = ex.getMessage
        if (msg.startsWith("ERROR: "))
          msg.split("\n")(0).substring(7)
        else msg
    }
}

object QuereasePostgresTests {
  Class.forName("org.postgresql.Driver") // fix "sbt +test" - No suitable driver found
  val conf = ConfigFactory.load()
  val hasPostgres = conf.getBoolean("querease.postgresql.available")
  def getPostgresConnection = {
    val url      = conf.getString("querease.postgresql.jdbc.url")
    val user     = conf.getString("querease.postgresql.user")
    val password = conf.getString("querease.postgresql.password")
    val conn     = DriverManager.getConnection(url, user, password)
    conn
  }
  def setPostgresEnv = setEnv(PostgresqlDialect, getPostgresConnection)
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
  val createPostgresObjectsStatements =
    Seq(
      "drop schema if exists querease cascade",
      "create schema querease authorization querease"
    ) ++
    SqlWriter.postgresql().schema(qe.tableMetadata.tableDefs).split(";").toList.map(_.trim).filter(_ != "") ++
    postgres_custom_functions_statements ++
    Seq("CREATE SEQUENCE seq START WITH 10000",
        "commit"
    )
}
