package test

import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.tresql.{Query, Resources, dialects}

import java.sql.{Connection, DriverManager}

class CursorsTests extends FlatSpec with Matchers with BeforeAndAfterAll {

  var conn: Connection = _
  implicit var resources: Resources = _

  override def beforeAll(): Unit = {
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    conn = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
    resources = new Resources {}
      .withConn(conn)
      .withDialect(QuereaseHsqldbTests.hsqlDialect)
      .withLogger(QuereaseDbTests.TresqlLogger)
  }

  case class CursorTest(cursorPrefix: String,
                        bindVars: Map[String, Any],
                        bindVarExtraction: String,
                        tresql: String,
                        result: List[Map[String, Any]])

  case class ViewCursorTest(testName:String,
                            view: String,
                            bindVars: Map[String, Any],
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
          ":name dept_name, (dept_emps de[de.__row_nr = dew.__row_nr_ref]{de.name}) emp_name," +
          "(dept_emps de[de.__row_nr = dew.__row_nr_ref]{job}) job," +
          "name work_name, hours}"
        , List(Map("dept_name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> "4", "emp_name" -> "scott"))
        )
      )
    )
  )

  import QuereaseTests.qe

  data foreach { case (name, testData) =>
    it should name in {
      testData foreach { case CursorTest(prefix, input, varExtraction, tresql, testResult) =>
        val inp =
          if (varExtraction == null) input else {
            val vars = varExtraction.split(",")
            qe.extractDataForVars(input, vars.toList)
          }
        if (tresql!= null) {
          val result = Query(qe.cursorsFromBindVars(inp, prefix) + " " + tresql, inp).toListOfMaps
          result should be (testResult)
        }
      }
    }
  }

  Map("banks" -> List(Map("code" -> "SWE", "name" -> "Swedbank"), Map("code" -> "DNB", "name" -> "Luminor")))
  Map("banks" -> List(Map("code" -> "SWE", "name" -> "Swedbank"), Map("code" -> "DNB", "name" -> "Luminor", "accounts" -> List(Map("billing_account" -> "AAA"), Map("billing_account" -> "BBB", "currencies" -> List())))))

  val cursorData: List[ViewCursorTest] =
    List(
      ViewCursorTest
      ( "have three empty cursors"
      , "country"
      , Map()
      , """(banks { 1 nr, count(*) c }(1) ++
          |banks_accounts { 2 nr, count(*) c }(1) ++
          |banks_accounts_currencies { 3 nr, count(*) c }(1)){count(*) c}#(1)""".stripMargin
      , List(Map("c" -> 0))
      )
    , ViewCursorTest
      ( "match three cursors row count"
      , "country"
      , Map("banks" ->
          List
          ( Map("code" -> "SWE", "name" -> "Swedbank")
          , Map("code" -> "DNB", "name" -> "Luminor", "accounts" ->
              List
              ( Map("billing_account" -> "AAA")
              , Map("billing_account" -> "BBB", "currencies" ->
                  List(Map("code" -> "LVL"), Map("name" -> "Eiro")))))
          )
        )
      , """(banks { 1 nr, count(*) c }(1) ++
          |banks_accounts { 2 nr, count(*) c }(1) ++
          |banks_accounts_currencies { 3 nr, count(*) c }(1)){nr, c}#(1)""".stripMargin
      , List(Map("nr" -> 1, "c" -> 2), Map("nr" -> 2, "c" -> 2), Map("nr" -> 3, "c" -> 2))
      )
      , ViewCursorTest
      ( "match cursor content"
        , "country"
        , Map("banks" ->
          List
            ( Map("code" -> "SWE", "name" -> "Swedbank")
              , Map("code" -> "DNB", "name" -> "Luminor", "accounts" ->
              List
              ( Map("billing_account" -> "AAA")
                , Map("billing_account" -> "BBB", "currencies" ->
                List(Map("code" -> "LVL"), Map("name" -> "Eiro")))))
            )
          )
        , """banks_accounts_currencies { code, name }#(null 1, 2)"""
        , List(Map("code" -> null, "name" -> "Eiro"), Map("code" -> "LVL", "name" -> null))
      )
      , ViewCursorTest
      ( "match three cursors data"
        , "country"
        , Map("code" -> "LV", "name" -> "Latvia",
            "banks" ->
            List
            ( Map("code" -> "SWE", "name" -> "Swedbank")
              , Map("code" -> "DNB", "name" -> "Luminor", "accounts" ->
              List
              ( Map("billing_account" -> "AAA")
                , Map("billing_account" -> "BBB", "currencies" ->
                List(Map("code" -> "USD"), Map("code" -> "EUR")))))
            )
          )
        , """((banks { :name || ' (' || :code || ')' parent, code name }#(1)) ++
            |(banks_accounts ba { (banks b[b.__row_nr = ba.__row_nr_ref] {b.code}) parent,
            | group_concat(billing_account)#(~billing_account) name }(parent)) ++
            |(banks_accounts_currencies cur {
            |  (banks_accounts ba[ba.__row_nr = cur.__row_nr_ref]
            |    { ba.billing_account || ' (' || (banks b[b.__row_nr = ba.__row_nr_ref] {b.code}) || ')' }) parent,
            |  group_concat(code)#(code) name
            |}(parent)))
            |{parent, name}#(1, 2)""".stripMargin
        , List
          ( Map("parent" -> "BBB (DNB)", "name" -> "EUR,USD")
          , Map("parent" -> "DNB", "name" -> "BBB,AAA")
          , Map("parent" -> "Latvia (LV)", "name" -> "DNB")
          , Map("parent" -> "Latvia (LV)", "name" -> "SWE")
          )
      )
      , ViewCursorTest
        ( "handle empty lookup view references"
        , "person_with_parents_1"
        , Map()
        , """(mother{count(name) c} ++ father{count(surname)}){sum(c) c}"""
        , List(Map("c" -> 0))
        )
      , ViewCursorTest
      ( "handle lookup view references"
      , "person_with_parents_1"
      , Map("mother" -> Map("name" -> "Jana"), "father" -> Map("name" -> "Joi"))
      , """(mother{ name } ++ father { name }){name}#(1)"""
      , List(Map("name" -> "Jana"), Map("name" -> "Joi"))
      )
    )

  cursorData foreach { case ViewCursorTest(test, view, data, tresql, testRes) =>
    "view bind var cursor" should test in {
      val res = Query(qe.cursorsFromViewBindVars(data, qe.viewDef(view)) + " " + tresql, data).toListOfMaps
      res should be (testRes)
    }
  }

  override def afterAll(): Unit = conn.close()
}
