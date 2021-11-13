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
    QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
    conn = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
    resources = new Resources {}
      .withConn(conn)
      .withDialect(QuereaseHsqldbTests.hsqlDialect)
      .withLogger(QuereaseDbTests.TresqlLogger)
  }

  case class ViewCursorTest(testName:String,
                            view: String,
                            bindVars: Map[String, Any],
                            tresql: String,
                            result: List[Map[String, Any]])

  import QuereaseTests.qe

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
    it should test in {
      val res = Query(qe.cursorsFromViewBindVars(data, qe.viewDef(view)) + " " + tresql, data).toListOfMaps
      res should be (testRes)
    }
  }

  override def afterAll(): Unit = conn.close()
}
