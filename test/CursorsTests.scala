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
                        tresql: String,
                        result: List[Map[String, Any]])

  val data: List[(String, List[CursorTest])]  =
    List(
    ( "match 1 cursor data"
    , List(CursorTest
        ( "dept" // cursors prefix
        , Map("name" -> "Sales", "loc" -> "Riga") //bind variables
        , ":name, :loc" // variable extraction
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
        , "(dept {name ||  ' ' || loc data} + dept_emps {name || ', ' || job}) {data} #(1)"
        , List(Map("data" -> "Sales Riga"), Map("data" -> "scott, analyst"), Map("data" -> "ziga, developer"))
        )
      , CursorTest("dept"
        , Map("emps" -> List(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")))
        , null
        , "dept_emps[job = 'developer'] {name, job} "
        , List(Map("name" -> "ziga", "job" -> "developer"))
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
        , "dept_emps[name = 'scott']{ :name dept_name, :loc loc, name, job}"
        , List(Map("dept_name" -> "Sales", "loc" -> "Riga", "name" -> "scott", "job" -> "analyst"))
        )
      , CursorTest( "dept"
        ,  Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
            List(Map("name" -> "scott", "work" ->
              List(Map("name" -> "meet", "hours" -> 4), Map("name" -> "review", "hours" -> 2)), "job" -> "analyst"),
              Map("name" -> "ziga", "job" -> "developer", "work" -> Nil)))
        , null
        , "dept_emps_work dew [hours > 2] {" +
          ":name dept_name, (dept_emps de[de.__id = dew.__parent_id]{de.name}) emp_name," +
          "(dept_emps de[de.__id = dew.__parent_id]{job}) job," +
          "name work_name, hours}"
        , List(Map("dept_name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> "4", "emp_name" -> "scott"))
        )
      )
    )
  )

  data foreach { case (name, testData) =>
    it should name in {
      testData foreach { case CursorTest(prefix, input, varExtraction, tresql, testResult) =>
        val inp =
          if (varExtraction == null) input else {
            val vars = varExtraction.split(",")
            querease.cursorDataForVars(input, vars.toList)
          }
        if (tresql!= null) {
          val result = Query(querease.cursorsFromBindVars(inp, prefix) + " " + tresql, inp).toListOfMaps
          result should be (testResult)
        }
      }
    }
  }

  override def afterAll(): Unit = conn.close()
}
