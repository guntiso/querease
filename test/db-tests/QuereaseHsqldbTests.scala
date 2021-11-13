package test

import java.sql.DriverManager
import org.mojoz.metadata.out.SqlGenerator
import org.tresql.CoreTypes
import org.tresql.QueryBuilder
import org.tresql.dialects.HSQLDialect
import QuereaseDbTests.{setEnv, executeStatements}
import QuereaseTests.qe

class QuereaseHsqldbTests extends QuereaseDbTests {
  import QuereaseHsqldbTests._
  override def dbName = "hsqldb"
  override def setEnv: Unit = setHsqldbEnv
  override def createDbObjects = executeStatements(createHsqldbObjectsStatements: _*)
}

object QuereaseHsqldbTests {
  QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
  // TODO clean up when tresql fixed
  val hsqlDialect: CoreTypes.Dialect = HSQLDialect orElse {
    case c: QueryBuilder#CastExpr => c.typ match {
      case "bigint" | "long" | "int" => s"convert(${c.exp.sql}, BIGINT)"
      case _ => c.exp.sql
    }
  }
  def getHsqldbConnection = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
  def setHsqldbEnv = setEnv(hsqlDialect, getHsqldbConnection)
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
    Seq(
      "create sequence seq start with 10000",
      "create schema car_schema",
      "set database collation \"Latvian\"",
    ) ++
    hsqldb_custom_functions_statements ++
    SqlGenerator.hsqldb().schema(qe.tableMetadata.tableDefs).split(";").toList.map(_.trim).filter(_ != "")
      // TODO fk accross schemas - upgrade hsqldb? Provide explicit schema?
      .filterNot(_ startsWith "alter table car_schema.person_car add constraint fk_person_car_person_id")
}
