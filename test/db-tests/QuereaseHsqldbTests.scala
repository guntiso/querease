package test

import java.sql.DriverManager
import mojoz.metadata.out.SqlWriter
import org.tresql.dialects.HSQLDialect
import QuereaseDbTests.{setEnv, executeStatements}
import QuereaseTests.qe

class QuereaseHsqldbTests extends QuereaseDbTests {
  import QuereaseHsqldbTests._
  override def setEnv: Unit = setHsqldbEnv
  override def createDbObjects = executeStatements(createHsqldbObjectsStatements: _*)
}

object QuereaseHsqldbTests {
  Class.forName("org.hsqldb.jdbc.JDBCDriver") // fix "sbt +test" - No suitable driver found
  def getHsqldbConnection = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
  def setHsqldbEnv = setEnv(HSQLDialect, getHsqldbConnection)
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
  val createHsqldbObjectsStatements =
    SqlWriter.hsqldb().schema(qe.tableMetadata.tableDefs).split(";").toList.map(_.trim).filter(_ != "") ++
    hsqldb_custom_functions_statements ++
    Seq("CREATE SEQUENCE seq START WITH 10000")
}
