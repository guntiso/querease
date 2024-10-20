package test

import java.sql.DriverManager
import org.mojoz.metadata.out.DdlGenerator
import org.tresql.CoreTypes
import org.tresql.QueryBuilder
import org.tresql.dialects.HSQLDialect
import QuereaseDbTests.{setEnv, executeStatements, MainDb, ExtraDb}
import QuereaseTests.qe

class QuereaseHsqldbTests extends QuereaseDbTests {
  import QuereaseHsqldbTests._
  override def dbName = "hsqldb"
  override def setEnv(db: String): Unit = setHsqldbEnv(db)
  override def createDbObjects(db: String) = createHsqldbObjects(db)
}

object QuereaseHsqldbTests {
  QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
  System.setProperty(
    "hsqldb.method_class_names",
    "test.HsqldbCustomFunctions.*"// allow access to our custom java functions
  )
  Thread.sleep(50) // allow property to be set for sure (fix unstable hsqldb tests)

  def getHsqldbConnection(db: String) = db match {
    case MainDb   => DriverManager.getConnection("jdbc:hsqldb:mem:my-main-memdb", "SA", "")
    case ExtraDb  => DriverManager.getConnection("jdbc:hsqldb:mem:my-xtra-memdb", "SA", "")
  }
  def setHsqldbEnv(db: String) = setEnv(db, HSQLDialect, getHsqldbConnection(db))
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
  def createHsqldbObjectsStatements(db: String) =
    Seq(
      "create sequence seq start with 10000",
      "create schema car_schema",
      "set database collation \"Latvian\"",
    ) ++
    hsqldb_custom_functions_statements ++
    DdlGenerator.hsqldb().schema(qe.tableMetadata.tableDefs.filter(_.db == db)).split(";").toList.map(_.trim).filter(_ != "")
      // TODO fk accross schemas - upgrade hsqldb? Provide explicit schema?
      .filterNot(_ startsWith "alter table car_schema.person_car add constraint fk_person_car_person_id")
  def createHsqldbObjects(db: String) = executeStatements(db, createHsqldbObjectsStatements(db): _*)
}
