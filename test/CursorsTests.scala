import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.mojoz.querease._
import org.scalatest.BeforeAndAfterAll
import org.tresql.{Query, Resources, dialects}
import test.QuereaseDbTests

import java.sql.{Connection, DriverManager}

class CursorsTests extends FlatSpec with Matchers with BeforeAndAfterAll {

  val querease = new Querease with ScalaDtoQuereaseIo

  var conn: Connection = _
  implicit var resoures: Resources = _

  override def beforeAll(): Unit = {
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    conn = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
    resoures = new Resources {}
      .withConn(conn)
      .withDialect(dialects.HSQLDialect)
      .withLogger(QuereaseDbTests.TresqlLogger)
  }

  case class CursorTest(cursorPrefix: String,
                        bindVars: Map[String, Any],
                        bindVarExtraction: String,
                        cursorData: Map[String, Vector[Map[String, Any]]],
                        tresql: String,
                        result: List[Map[String, Any]])

  val data: List[(String, List[CursorTest])]  =
    List(
    ( "match 1 cursor data"
    , List(CursorTest
        ( "dept" // cursors prefix
        , Map("name" -> "Sales", "loc" -> "Riga") //bind variables
        , ":name, :loc" // variable extraction
        , Map("dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga"))) // bind variables as cursor data
        , "dept {name || ' ' || loc name}#(1)" // test tresql
        ,  List(Map("name" -> "Sales Riga")) //test result
        )
      )
    ),
    ( "match 2 cursor data"
    , List(CursorTest
        ( "dept"
        , Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
          List(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")))
        , null
        , Map("dept_emps" -> Vector(
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
            "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
        , "(dept {name ||  ' ' || loc data} + dept_emps {emps_name || ', ' || job}) #(1)"
        , List(Map("DATA" -> "Sales Riga"), Map("DATA" -> "scott, analyst"), Map("DATA" -> "ziga, developer"))
        )
      , CursorTest("dept"
        , Map("emps" -> List(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")))
        , null
        , Map("dept_emps" ->
            Vector(
              Map("name" -> "scott", "job" -> "analyst"),
              Map("name" -> "ziga", "job" -> "developer")),
            "dept" -> Vector())
        , null
        , null
        )
      )
    ),
    ("match 3 cursor data"
    , List(CursorTest
        ( "dept"
        , Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
          List(Map("name" -> "scott", "work" ->
            List(Map("name" -> "meet", "hours" -> 4), Map("name" -> "review", "hours" -> 2)), "job" -> "analyst"),
            Map("name" -> "ziga", "job" -> "developer", "work" -> List(Map("name" -> "meet", "hours" -> 4)))))
        , null
        , Map("dept_emps_work" ->
          Vector(
            Map("name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> 4, "emps_name" -> "scott", "loc" -> "Riga"),
            Map("name" -> "Sales", "work_name" -> "review", "job" -> "analyst", "hours" -> 2, "emps_name" -> "scott", "loc" -> "Riga"),
            Map("name" -> "Sales", "work_name" -> "meet", "job" -> "developer", "hours" -> 4, "emps_name" -> "ziga", "loc" -> "Riga")),
          "dept_emps" ->
            Vector(
              Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
              Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
          "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
        , "dept_emps[emps_name = 'scott']"
        , List(Map("NAME" -> "Sales", "LOC" -> "Riga", "EMPS_NAME" -> "scott", "JOB" -> "analyst"))
        )
      , CursorTest( "dept"
        ,  Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
            List(Map("name" -> "scott", "work" ->
              List(Map("name" -> "meet", "hours" -> 4), Map("name" -> "review", "hours" -> 2)), "job" -> "analyst"),
              Map("name" -> "ziga", "job" -> "developer", "work" -> Nil)))
        , null
        , Map("dept_emps_work" ->
            Vector(
              Map("name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> 4, "emps_name" -> "scott", "loc" -> "Riga"),
              Map("name" -> "Sales", "work_name" -> "review", "job" -> "analyst", "hours" -> 2, "emps_name" -> "scott", "loc" -> "Riga")),
            "dept_emps" ->
            Vector(
              Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
              Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
            "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
        , "dept_emps_work [hours > 2] {name, work_name, job, hours, emps_name}#(1,2,3,4,5)"
        , List(Map("name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> "4", "emps_name" -> "scott"))
        )
      )
    )
  )

  data foreach { case (name, testData) =>
    it should name in {
      testData foreach { case CursorTest(prefix, input, varExtraction, testCursorData, tresql, testResult) =>
        val inp =
          if (varExtraction == null) input else {
            val vars = varExtraction.split(",")
            querease.cursorDataForVars(input, vars.toList)
          }
        val cursorData = querease.cursorData(inp, prefix, Map())
        cursorData should be (testCursorData)
        if (tresql!= null) {
          val result = Query(querease.cursors(cursorData) + " " + tresql, cursorData).toListOfMaps
          result should be (testResult)
        }
      }
    }
  }

  override def afterAll(): Unit = conn.close()
}
