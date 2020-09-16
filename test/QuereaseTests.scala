package test

import java.io.PrintWriter
import java.sql.{Connection, DriverManager, Timestamp}
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.tresql._
import org.tresql.dialects.HSQLDialect
import dto._
import org.mojoz.metadata._
import org.mojoz.metadata.TableMetadata
import org.mojoz.metadata.in.YamlMd
import org.mojoz.metadata.in.YamlTableDefLoader
import org.mojoz.metadata.in.YamlViewDefLoader
import org.mojoz.metadata.out.SqlWriter
import org.mojoz.querease._


class QuereaseTests extends FlatSpec with Matchers {
  import QuereaseTests._
  "objects" should "produce correct save-to maps" in {
    def asKeys(instance: Dto) =
      qe.toSaveableMap(instance).toList.sortBy(_._1).map {
        case (k, v) if k endsWith "->" => k + v
        case (k, v) => k
      }
    def keys(instance: Dto) =
      asKeys(instance).mkString("; ")
    def resolverKeys(instance: Dto) =
      asKeys(instance).filter(_.indexOf("->") >= 0).toList.sorted.mkString("; ")

    keys(new ResolverTestAccount1) should be(List(
      "code",
      "code->bank_id=checked_resolve(_, array(bank[code = _]{id}@(2))," +
          " 'Failed to identify value of \"code\" (from resolver_test_account_1) - ' || coalesce(_, 'null'))",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccount2) should be(List(
      "code",
      "code->bank_id=checked_resolve(coalesce(:code, :some_other_variable::text), array(bank[code = :code && :some_other_variable]{id}@(2))," +
          " 'Failed to identify value of \"code\" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable::text, 'null')))",
      "id"
    ).mkString("; "))
    keys(new ResolverTestAccountSelfRef1) should be(List(
      "name",
      "name->id=checked_resolve(_, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = _]{account.id}@(2))," +
          " 'Failed to identify value of \"name\" (from resolver_test_account_self_ref_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestBank1) should be("name->name='My bank'")
    resolverKeys(new ResolverTestBank2) should be("name->name=_ || ' saved'")
    keys(new ResolverTestAccountCurrency1) should be(List(
      "account",
      "account->account_id=checked_resolve(_, array(account[billing_account = _]{id}@(2))," +
          " 'Failed to identify value of \"account\" (from resolver_test_account_currency_1) - ' || coalesce(_, 'null'))",
      "currency_name",
      "currency_name->currency_code=checked_resolve(_, array(currency[name = _]{code}@(2))," +
          " 'Failed to identify value of \"currency_name\" (from resolver_test_account_currency_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson1) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || surname = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_1) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || surname = _]{id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_1) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson2) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#1)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_2) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson3) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#4)' = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_3) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#2)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_3) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson4) should be(List(
      "father->father_id=2",
      "mother->mother_id=1"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson5) should be(List(
      "father->father_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#7)' = _]{id}@(2))," +
          " 'Failed to identify value of \"father\" (from resolver_test_person_5) - ' || coalesce(_, 'null'))",
      "mother->mother_id=checked_resolve(_, array(person[name || ' ' || surname || ' (#5)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_5) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson6) should be(List(
      "father->father_id=4",
      "mother->mother_id=3"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson7) should be(List(
      "mother->mother_id=checked_resolve(_, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = _]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_7) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new ResolverTestPerson8) should be(List(
      "mother->mother_id=checked_resolve(_, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = _]{p1.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from resolver_test_person_8) - ' || coalesce(_, 'null'))"
    ).mkString("; "))
    resolverKeys(new NestedResolverTest1) should be(List(
      "mother->mother_id=checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother &" +
        " person.father_id = checked_resolve(:other_field::text, array(" +
          "person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2))," +
          " 'Failed to identify value of \"other_field\" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field::text, 'null'))]]{person.id}@(2))," +
          " 'Failed to identify value of \"mother\" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))"
    ).mkString("; "))
  }
  "querease" should "select referenced fields correctly" in {
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_2"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_expression_test_person_2b"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_expression_test_person_2c"), Map.empty)._1 should be(
      "person p2 {" +
      "p2.id, " +
      "(person[person.id = p2.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p2.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_3"), Map.empty)._1 should be(
      "person p3 {" +
      "p3.id, " +
      "(person[p3.mother_id = id + 3]{person.name || ' ' || person.surname || ' (#2)' full_name}) mother, " +
      "(person[person.id = p3.father_id]{person.name || ' ' || person.surname || ' (#3)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_5"), Map.empty)._1 should be(
      "person p5 {" +
      "p5.id, " +
      "(person[p5.mother_id = id + 5]{person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p5.father_id]{person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_6"), Map.empty)._1 should be(
      "person p6 {" +
      "p6.id, " +
      "(person[person.id = p6.mother_id]{person.name || ' ' || person.surname || ' (#5)' full_name}) mother, " +
      "(person[person.id = p6.father_id]{person.name || ' ' || person.surname || ' (#6)' full_name}) father}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_7"), Map.empty)._1 should be(
      "person p7 {" +
      "(person;person[person.father_id]person? father[person.id = p7.mother_id]{person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_test_person_8"), Map.empty)._1 should be(
      "person p8 {" +
      "(person p1;p1[p1.father_id]person? father[p1.id = p8.mother_id]{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_2"), Map.empty)._1 should be(
      "bank {" +
      "bank.name, " +
      "(country[country.code = bank.country_code]{country.code || ' - ' || country.name c2_and_name}) country}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_3"), Map.empty)._1 should be(
      "bank b3 {" +
      "b3.name, " +
      "(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )

    // test alias clash resolvement in implicit resolver joins
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_7_a"), Map.empty)._1 should be(
      "person {" +
      "(person_2 (# mother_id) { null{person.mother_id} } person_2[person.id = person_2.mother_id]person;" +
      "person[person.father_id]person? father{person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_8_a"), Map.empty)._1 should be(
      "person p1 {" +
      "(p1_2 (# mother_id) { null{p1.mother_id} } p1_2[p1.id = p1_2.mother_id]person p1;" +
      "p1[p1.father_id]person? father{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )
    qe.queryStringAndParams(qe.viewDef("resolver_alias_clash_test_person_8_b"), Map.empty)._1 should be(
      "person father {" +
      "(father_2 (# mother_id) { null{father.mother_id} } father_2[p1.id = father_2.mother_id]person p1;" +
      "p1[p1.father_id]person? father{p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' full_name}) mother}"
    )

    // test implied join to self
    qe.queryStringAndParams(qe.viewDef("self_ref_test_account_1"), Map.empty)._1 should be(
      "account {(account_2 (# id) { null{account.id} } account_2[account.id = account_2.id]account;" +
      "account/bank?{bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("self_ref_test_account_2"), Map.empty)._1 should be(
      "account a1 {(account;account/bank?[account.id = a1.id]{bank.code || ', ' || bank.name || ', ' || account.id name}) full_name}"
    )

    // test ambiguity resolver
    qe.queryStringAndParams(qe.viewDef("ambiguity_resolver_test_person_1"), Map.empty)._1 should be(
      "person p1 {" +
      "(person[person.id = p1.id]{person.name || ' ' || person.surname || ' (#1)' full_name}) this_name, " +
      "(person[person.id = p1.father_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) fath_name, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) moth_name, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) mother, " +
      "(person[person.id = p1.mother_id]{person.name || ' ' || person.surname || ' (#1)' full_name}) ma}"
    )

    // test resolver in filter
    qe.queryStringAndParams(qe.viewDef("filter_with_resolver_test_1"), Map("mother" -> "mother"))._1 should be(
      "person" +
      "[mother_id = checked_resolve(if_defined_or_else(:mother?, :mother?::text, null)," +
      " array(person;person[person.mother_id]person? mother[" +
      "[if_defined_or_else(:mother?, mother.name || mother.surname = :mother & person.id::text ~ '%6', false)]]{person.mother_id}@(2))," +
      " 'Failed to identify value of \"mother\" (from filter_with_resolver_test_1) - ' ||" +
      " if_defined_or_else(:mother?, coalesce(:mother?::text, 'null'), '[missing]'))" +
      "] {person.name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_resolver_test_2"), Map("mother" -> "mother"))._1 should be(
      "person" +
      "[person[mother_id = checked_resolve(if_defined_or_else(:mother?, :mother?::text, null)," +
      " array(person[[person.name || ' ' || person.surname || ' (#1)' = :mother?]]{person.id}@(2))," +
      " 'Failed to identify value of \"mother\" (from filter_with_resolver_test_2) - ' ||" +
      " if_defined_or_else(:mother?, coalesce(:mother?::text, 'null'), '[missing]'))]{1}" +
      "] {person.name}"
    )

    // test field-ref in filter
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_4"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :country_c3_and_name?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_5"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :country_choice_3_c3_and_name?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("ref_test_bank_6"), Map.empty)._1 should be(
      "bank b3[(country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) = :cc?] " +
      "{b3.name, (country c3[c3.code = b3.country_code]{c3.code3 || ' - ' || c3.name c3_and_name}) country_c3_and_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_1"), Map("full_name" -> "full_name"))._1 should be(
      "person[person.name || ' ' || person.surname = :full_name?] {person.name || ' ' || person.surname full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_2"), Map("full_name" -> "full_name"))._1 should be(
      "person[person.name || ' ' || person.surname = :full_name] {person.name || ' ' || person.surname full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_3"), Map("full_name" -> "full_name"))._1 should be(
      "person[(person_2 (# id) { null{person.id} } person_2[person.id = person_2.id]person{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person_2 (# id) { null{person.id} } person_2[person.id = person_2.id]person{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_4"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_5"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.father_id]{person.name || ' ' || person.surname full_name}) father_full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_6"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.father_id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
    qe.queryStringAndParams(qe.viewDef("filter_with_field_ref_test_7"), Map("full_name" -> "full_name"))._1 should be(
      "person p1[(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) = :full_name] " +
      "{(person[person.id = p1.id]{person.name || ' ' || person.surname full_name}) full_name}"
    )
  }
  "implicit querease" should "be found" in {
    val c1 = new dto.PersonName
    c1.name = "Simona"
    val c2 = new dto.PersonName
    c2.name = "Sigita"
    val pi = new dto.PersonInfo
    pi.name = "Gunzi"
    pi.father_name = "Juris"
    pi.sex = "V"
    pi.children = List(c1, c2);
    pi.toString should be(
      "dto.PersonInfo{name: Gunzi, surname: null, sex: V, mother_name: null, father_name: Juris, " +
      "maternal_grandmother: null, maternal_grandfather: null, paternal_grandmother: null, paternal_grandfather: null, " +
      "children: (dto.PersonName{name: Simona}, dto.PersonName{name: Sigita}), father: null}"
    )
    pi.toMap should be(
      Map("name" -> "Gunzi", "surname" -> null, "sex" -> "V", "mother_name" -> null, "father_name" -> "Juris",
          "maternal_grandmother" -> null, "maternal_grandfather" -> null, "paternal_grandmother" -> null, "paternal_grandfather" -> null,
          "children" -> List(Map("name" -> "Simona"), Map("name" -> "Sigita")), "father" -> null))
  }

  "dto generator" should "generate correct dto code" in {
    val generator = new ScalaDtoGenerator(qe)
    val dtosPath = "test/dtos"
    val expectedDtos: String = fileToString(dtosPath + "/" + "dtos-out.scala")
    val producedDtos: String = generator.generateScalaSource(Nil, qe.nameToViewDef.values.toList.sortBy(_.name), Nil)
    if (expectedDtos != producedDtos)
      toFile(dtosPath + "/" + "dtos-out-produced.scala", producedDtos)
    expectedDtos shouldBe producedDtos
  }

  "tresql metadata" should "report correct table metadata" in {
    val md = new TresqlMetadata(qe.tableMetadata.tableDefs)
    val mdPath = "test/metadata"
    val mdFilePath: String = mdPath + "/" + "tresql-table-metadata-out.yaml"
    val expectedMd: String = fileToString(mdFilePath)
    val producedMd: String = md.tableMetadataString
    if (expectedMd != producedMd)
      toFile(mdPath + "/" + "tresql-table-metadata-out-produced.yaml", producedMd)
    expectedMd shouldBe producedMd
    // roundtrip test
    val mdFromFile = (new TresqlMetadataFactory).create(Map("tableMetadataFile" -> mdFilePath)).metadata.asInstanceOf[TresqlMetadata]
    val roundtripProducedMd = mdFromFile.tableMetadataString
    if (expectedMd != roundtripProducedMd)
      toFile(mdPath + "/" + "tresql-table-metadata-out-produced-round.yaml", roundtripProducedMd)
    expectedMd shouldBe roundtripProducedMd
  }
}

object Naming {
  def dbName(name: String) = {
    val parts = dasherize(name).split("[\\-\\_]")
    val hadPrefix = name.startsWith("_")
    val hadSuffix = name.endsWith("_")
    val clean = parts.toList
      .map(_.toLowerCase)
      .mkString("_")
      .replace("__", "_")
    (hadPrefix, hadSuffix) match {
      case (true, true) => "_" + clean + "_"
      case (true, false) => "_" + clean
      case (false, true) => clean + "_"
      case (false, false) => clean
    }
  }
  def dasherize(name: String) = {
    val (upper, digit, other) = (1, 2, 3)
    val buf = new StringBuilder(name.length() * 2)
    var charType = 0
    var prevCharType = 0
    for (i <- 0 to (name.length - 1)) {
      val c = name.charAt(i)
      if (Character.isUpperCase(c)) charType = upper
      else if (Character.isDigit(c)) charType = digit
      else charType = other
      if (i > 0
        && charType != prevCharType
        && !(prevCharType == upper && charType == other)) {
        buf.append('-')
      }
      if (charType == upper) buf.append(Character.toLowerCase(c))
      else buf.append(c)
      prevCharType = charType
    }
    buf.toString
  }
}

object QuereaseTests {
  def dbName(name: String) =
    Naming.dbName(name)
      .replace("_1", "1") // no underscore before 1 in our database names
      .replace("_2", "2") // no underscore before 2 in our database names
      .replace("_3", "3") // no underscore before 3 in our database names
      .replace("_4", "4") // no underscore before 4 in our database names
      .replace("_5", "5") // no underscore before 5 in our database names
      .replace("_6", "6") // no underscore before 6 in our database names
      .replace("_7", "7") // no underscore before 7 in our database names
      .replace("_8", "8") // no underscore before 8 in our database names
      .replace("_9", "9") // no underscore before 9 in our database names

   object TestQuerease extends Querease with ScalaDtoQuereaseIo {

     override type DTO = Dto

     override lazy val tableMetadata =
       new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions).tableDefs, dbName)
     override lazy val yamlMetadata =
        YamlMd.fromFiles(path = "test/tables") ++
        YamlMd.fromFiles(path = "test/views")
     override lazy val nameToViewDef = YamlViewDefLoader(
       tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions)
       .nameToViewDef.toMap
     override protected lazy val viewNameToFieldOrdering =
       nameToViewDef.map(kv => (kv._1, new FieldOrdering(
         kv._2.fields
          .map(f => Option(f.alias) getOrElse f.name)
          .zipWithIndex.toMap)
       ))
     override def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
       Naming.dasherize(mf.runtimeClass.getSimpleName).replace("-", "_")
   }
  implicit val qe = TestQuerease
  val nl = System.getProperty("line.separator")
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body.replace(nl, "\n") // normalize newlines
  }
  def toFile(filename: String, message: String): Unit = {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}
