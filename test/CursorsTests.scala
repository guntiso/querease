package test

import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.tresql.{Query, QueryParser, Resources, dialects}

import java.sql.{Connection, DriverManager}

class CursorsTests extends FlatSpec with Matchers with BeforeAndAfterAll {

  var conn: Connection = _
  implicit var resources: Resources = _

  override def beforeAll(): Unit = {
    QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
    conn = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
    resources = new Resources {}
      .withConn(conn)
      .withMacros(null)
      .withDialect(QuereaseHsqldbTests.hsqlDialect)
      .withLogger(QuereaseDbTests.TresqlLogger)
  }

  case class ViewCursorTest(testName:String,
                            view: String,
                            bindVars: Map[String, Any],
                            tresql: String,
                            result: List[Map[String, Any]])

  import QuereaseTests.qe

  val cursorData: List[ViewCursorTest] =
    List(
      ViewCursorTest
      ( "have three empty cursors"
      , "country"
      , Map()
      , """[build_cursors()](banks { 1 nr, count(*) c }(1) ++
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
              , Map("billing_account" -> "BBB", "currencies" -> List(Map("code" -> "LVL"), Map("name" -> "Eiro")))
              )
            )
          )
        )
      , """[build_cursors()](banks { 1 nr, count(*) c }(1) ++
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
                List(Map("currency_code" -> "LVL"), Map("currency_name" -> "Eiro")))))
            )
          )
        , """[build_cursors()]banks_accounts_currencies { currency_code, currency_name }#(null 1, 2)"""
        , List(Map("currency_code" -> null, "currency_name" -> "Eiro"), Map("currency_code" -> "LVL", "currency_name" -> null))
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
                List(Map("currency_code" -> "USD"), Map("currency_code" -> "EUR")))))
            )
          )
        , """[build_cursors()]((banks { :name || ' (' || :code || ')' parent, code name }#(1)) ++
            |(banks_accounts ba { (banks b[b.__row_nr = ba.__row_nr_ref] {b.code}) parent,
            | group_concat(billing_account)#(~billing_account) name }(parent)) ++
            |(banks_accounts_currencies cur {
            |  (banks_accounts ba[ba.__row_nr = cur.__row_nr_ref]
            |    { ba.billing_account || ' (' || (banks b[b.__row_nr = ba.__row_nr_ref] {b.code}) || ')' }) parent,
            |  group_concat(currency_code)#(currency_code) name
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
        , """[build_cursors()](mother{count(name) c} ++ father{count(surname)}){sum(c) c}"""
        , List(Map("c" -> 0))
        )
      , ViewCursorTest
      ( "handle lookup view references"
      , "person_with_parents_1"
      , Map("mother" -> Map("name" -> "Jana"), "father" -> Map("name" -> "Joi"))
      , """[build_cursors()](mother{ name } ++ father { name }){name}#(1)"""
      , List(Map("name" -> "Jana"), Map("name" -> "Joi"))
      )
      , ViewCursorTest
      ("handle list data"
        , null
        , Map("currencies" -> List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar")))
        , """[build_cursors(:currencies, currency)]currencies{code, name}#(1)"""
        , List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar"))
      )
    )

  cursorData foreach { case ViewCursorTest(test, view, data, tresql, testRes) =>
    it should test in {
      val val_tresql = qe.maybeExpandWithBindVarsCursorsForView(tresql, data)(
        Option(view).map(qe.viewDef).orNull, new QueryParser(null, null))
      val res = Query(val_tresql, data).toListOfMaps
      res should be (testRes)
    }
  }

  override def afterAll(): Unit = conn.close()
}
