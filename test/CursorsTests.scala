package test

import org.mojoz.querease.QuereaseMacros
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.tresql.dialects.HSQLDialect
import org.tresql.{MacroResourcesImpl, Query, QueryCompiler, Resources}

import java.sql.{Connection, DriverManager}

class CursorsTests extends FlatSpec with Matchers with BeforeAndAfterAll {

  var conn: Connection = _
  implicit var resources: Resources = _

  override def beforeAll(): Unit = {
    QuereaseDbTests.loadJdbcDrivers // fix "sbt +test" - No suitable driver found
    conn = DriverManager.getConnection("jdbc:hsqldb:mem:mymemdb", "SA", "")
    resources = new Resources {}
      .withConn(conn)
      .withMacros(new QuereaseMacros)
      .withMetadata(QuereaseTests.qe.tresqlMetadata)
      .withDialect(HSQLDialect)
      .withLogger(QuereaseDbTests.TresqlLogger)
  }

  case class ViewCursorTest(testName:String,
                            bindVars: Map[String, Any],
                            tresql: String,
                            result: List[Map[String, Any]])

  import QuereaseTests.qe

  val cursorData: List[ViewCursorTest] =
    List(
      ViewCursorTest
      ( "have three empty cursors"
      , Map()
      , """[build_cursors(country)](banks { 1 nr, count(*) c }(1) ++
          |banks_accounts { 2 nr, count(*) c }(1) ++
          |banks_accounts_currencies { 3 nr, count(*) c }(1)){count(*) c}#(1)""".stripMargin
      , List(Map("c" -> 0))
      )
    , ViewCursorTest
      ( "match three cursors row count"
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
      , """[build_cursors(country)](banks { 1 nr, count(*) c }(1) ++
          |banks_accounts { 2 nr, count(*) c }(1) ++
          |banks_accounts_currencies { 3 nr, count(*) c }(1)){nr, c}#(1)""".stripMargin
      , List(Map("nr" -> 1, "c" -> 2), Map("nr" -> 2, "c" -> 2), Map("nr" -> 3, "c" -> 2))
      )
      , ViewCursorTest
      ( "match cursor content"
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
        , """[build_cursors(country)]banks_accounts_currencies { currency_code, currency_name }#(null 1, 2)"""
        , List(Map("currency_code" -> null, "currency_name" -> "Eiro"), Map("currency_code" -> "LVL", "currency_name" -> null))
      )
      , ViewCursorTest
      ( "match three cursors data"
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
        , """[build_cursors(country)]((banks { :name || ' (' || :code || ')' parent, code name }#(1)) ++
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
        , Map()
        , """[build_cursors(person_with_parents_1)](mother{count(name) c} ++ father{count(surname)}){sum(c) c}"""
        , List(Map("c" -> 0))
        )
      , ViewCursorTest
      ( "handle lookup view references"
      , Map("mother" -> Map("name" -> "Jana"), "father" -> Map("name" -> "Joi"))
      , """[build_cursors(person_with_parents_1)](mother{ name } ++ father { name }){name}#(1)"""
      , List(Map("name" -> "Jana"), Map("name" -> "Joi"))
      )
      , ViewCursorTest
      ("handle list data"
        , Map("currencies" -> List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar")))
        , """[build_cursors(currency, :currencies)]currencies{code, name}#(1)"""
        , List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar"))
      )
      , ViewCursorTest
      ("handle list data with view name as string literal"
        , Map("currencies" -> List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar")))
        , """[build_cursors('currency', :currencies)]currencies{code, name}#(1)"""
        , List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar"))
      )
      , ViewCursorTest
      ( "handle build cursors in with query"
        , Map("mother" -> Map("name" -> "Jana"), "father" -> Map("name" -> "Joi"))
        , """mf(#name) { {(mother{ name }) || ',' || (father { name })} } [build_cursors(person_with_parents_1)]mf{name}"""
        , List(Map("name" -> "Jana,Joi"))
      )
      , ViewCursorTest
      ("handle 2 par build cursors in with query"
        , Map("currencies" -> List(Map("code" -> "EUR", "name" -> "Euro"), Map("code" -> "USD", "name" -> "US dollar")))
        , """cc(# c) { currencies {count(*)} } [build_cursors(currency, :currencies)] cc{c}"""
        , List(Map("c" -> 2))
      )
    )

  cursorData foreach { case ViewCursorTest(test, data, tresql, testRes) =>
    it should test in {
      val res = Query(tresql, data).toListOfMaps
      res should be (testRes)
    }
  }

  it should "not throw StackOverflowError" in {
    val data = Map(
      ("currencies", (1 to (1024 * 16)).map(i => Map("code" -> s"c$i", "name" -> s"n$i")).toList)
    )
    Query.build("[build_cursors(currency, :currencies)] currencies{count(*)}", data).sql.length should be > 0
    Query.build("cmin(# min){currencies{min(code) min}}," +
      "cmax(# max) {currencies{max(code) max}}, ccount(# count) {currencies{count(code) count}} " +
      "[build_cursors(currency, :currencies)] cmax[]cmin[]ccount{max, min, count}", data).sql.length should be > 0
  }

  it should "compile cursors" in {
    def compileQueries = {
      val compiler = new QueryCompiler(
        QuereaseTests.qe.tresqlMetadata,
        Map(),
        new MacroResourcesImpl(new QuereaseMacros, QuereaseTests.qe.tresqlMetadata)
      )
      cursorData.foreach { ct =>
        try compiler.compile(ct.tresql) catch {
          case e: Exception =>
            throw new RuntimeException(s"Failed to compile tresql for test '${ct.testName}'", e)
        }
      }
      "done"
    }
    compileQueries shouldBe "done"
  }

  override def afterAll(): Unit = conn.close()
}
