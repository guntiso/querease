class account extends DtoWithId {
  var id: java.lang.Long = null
  var bank_id: java.lang.Long = null
  var billing_account: String = null
  var currency: List[String] = Nil
}
class account_choice extends account with DtoWithId {
  var id: java.lang.Long = null
  var bank_id: java.lang.Long = null
  var billing_account: String = null
  var currency: List[String] = Nil
  var name: String = null
}
class account_currency extends Dto {
  var account_id: java.lang.Long = null
  var currency_code: String = null
}
class account_details extends account with DtoWithId {
  var id: java.lang.Long = null
  var bank_id: java.lang.Long = null
  var billing_account: String = null
  var currency: List[String] = Nil
  var bank_code: String = null
  var bank_name: String = null
}
class account_with_bank extends DtoWithId {
  var id: java.lang.Long = null
  var billing_account: String = null
  var last_modified: java.sql.Timestamp = null
  var bank: account_with_bank_bank = null
  def resolve_bank_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:bank.id}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object account_with_bank {
  def resolve_bank_id(bank: account_with_bank_bank)(implicit env: org.tresql.Resources) = {
    tresql"""{:bank.id}"""(env.withParams(Map("bank" -> Option(bank).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
}
class account_with_bank_2 extends DtoWithId {
  var id: java.lang.Long = null
  var billing_account: String = null
  var bank: account_with_bank_2_bank = null
}
class account_with_bank_2_bank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var country_code: String = null
}
class account_with_bank_bank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var country_code: String = null
}
class ambiguity_resolver_test_person_1 extends Dto {
  var this_name: String = null
  var fath_name: String = null
  var moth_name: String = null
  var mother: String = null
  var ma: String = null
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ambiguity_resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:ma, array(person[name || ' ' || surname || ' (#1)' = :ma]{person.id}@(2)), 'Failed to identify value of "ma" (from ambiguity_resolver_test_person_1) - ' || coalesce(:ma, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object ambiguity_resolver_test_person_1 {
  def resolve_father_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ambiguity_resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_mother_id(ma: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:ma, array(person[name || ' ' || surname || ' (#1)' = :ma]{person.id}@(2)), 'Failed to identify value of "ma" (from ambiguity_resolver_test_person_1) - ' || coalesce(:ma, 'null'))}"""(env.withParams(Map("ma" -> ma)))
      .unique[java.lang.Long]
  }
}
class bank_list_row extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var country_code: String = null
  var country_name: String = null
  var name: String = null
}
class bank_list_row_with_filter extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class bank_list_row_with_group extends Dto {
  var name: String = null
}
class bank_list_row_with_having extends Dto {
  var name: String = null
  var total: java.lang.Integer = null
}
class bank_with_accounts_1 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
  var accounts: List[bank_with_accounts_1_accounts] = Nil
}
class bank_with_accounts_1_accounts extends DtoWithId {
  var id: java.lang.Long = null
  var billing_account: String = null
  var last_modified: java.sql.Timestamp = null
  def resolve_last_modified(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{now()}"""(env.withParams(this.toMap))
      .unique[java.sql.Timestamp]
  }
}
object bank_with_accounts_1_accounts {
  def resolve_last_modified(implicit env: org.tresql.Resources) = {
    tresql"""{now()}"""(env.withParams(Map.empty))
      .unique[java.sql.Timestamp]
  }
}
class car_and_person_01 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_02 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_03 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_04 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_05 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_06 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_07 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_08 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_09 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class car_and_person_10 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class country extends Dto {
  var code: String = null
  var name: String = null
  var is_active: java.lang.Boolean = null
  var banks: List[country_banks] = Nil
}
class country_banks extends Dto {
  var code: String = null
  var name: String = null
  var accounts: List[country_banks_accounts] = Nil
}
class country_banks_accounts extends Dto {
  var billing_account: String = null
  var currencies: List[country_banks_accounts_currencies] = Nil
}
class country_banks_accounts_currencies extends Dto {
  var currency_code: String = null
  var currency_name: String = null
}
class country_choice_2 extends Dto {
  var c2_and_name: String = null
}
class country_choice_3 extends Dto {
  var c3_and_name: String = null
}
class currency extends Dto {
  var code: String = null
  var name: String = null
  var name_eng: String = null
  var name_rus: String = null
  def resolve_name_eng(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:name_eng, '')}"""(env.withParams(this.toMap))
      .unique[String]
  }
  def resolve_name_rus(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:name_rus, '')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object currency {
  def resolve_name_eng(name_eng: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:name_eng, '')}"""(env.withParams(Map("name_eng" -> name_eng)))
      .unique[String]
  }
  def resolve_name_rus(name_rus: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:name_rus, '')}"""(env.withParams(Map("name_rus" -> name_rus)))
      .unique[String]
  }
}
class father_tree extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var sons: List[father_tree] = Nil
}
class filter_with_field_ref_test_1 extends Dto {
  var full_name: String = null
}
class filter_with_field_ref_test_2 extends Dto {
  var full_name: String = null
}
class filter_with_field_ref_test_3 extends Dto {
  var full_name: String = null
}
class filter_with_field_ref_test_4 extends Dto {
  var full_name: String = null
}
class filter_with_field_ref_test_5 extends Dto {
  var father_full_name: String = null
}
class filter_with_field_ref_test_6 extends Dto {
  var full_name: String = null
}
class filter_with_field_ref_test_7 extends Dto {
  var full_name: String = null
}
class filter_with_resolver_test_1 extends Dto {
  var name: String = null
}
class filter_with_resolver_test_2 extends Dto {
  var name: String = null
}
class filter_with_resolver_test_3_a extends Dto {
  var name: String = null
}
class filter_with_resolver_test_3_b extends Dto {
  var name: String = null
}
class filter_with_resolver_test_4 extends Dto {
  var name: String = null
}
class mother extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var sex: String = null
  var daughters: List[mother_daughters] = Nil
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{'F'}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object mother {
  def resolve_sex(implicit env: org.tresql.Resources) = {
    tresql"""{'F'}"""(env.withParams(Map.empty))
      .unique[String]
  }
}
class mother_daughters extends Dto {
  var name: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{'F'}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object mother_daughters {
  def resolve_sex(implicit env: org.tresql.Resources) = {
    tresql"""{'F'}"""(env.withParams(Map.empty))
      .unique[String]
  }
}
class nested_resolver_test_1 extends Dto {
  var other_field: String = null
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother & person.father_id = checked_resolve(:other_field::text, array(person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2)), 'Failed to identify value of "other_field" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field::text, 'null'))]]{person.id}@(2)), 'Failed to identify value of "mother" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object nested_resolver_test_1 {
  def resolve_mother_id(mother: String, other_field: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother & person.father_id = checked_resolve(:other_field::text, array(person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2)), 'Failed to identify value of "other_field" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field::text, 'null'))]]{person.id}@(2)), 'Failed to identify value of "mother" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))}"""(env.withParams(Map("mother" -> mother, "other_field" -> other_field)))
      .unique[java.lang.Long]
  }
}
class noid_test extends DtoWithId {
  var id: java.lang.Long = null
  var nm: String = null
}
class noid_test_2 extends Dto {
  var no_id: java.lang.Long = null
  var no_nm: String = null
}
class optional_params_resolver_test_1 extends DtoWithId {
  var id: java.lang.Long = null
  var mother_id: java.lang.Long = null
  var father_id: java.lang.Long = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(if_defined_or_else(:mother_id?, :mother_id::bigint, :father_id::bigint), if_defined_or_else(:id?, :id?::bigint, -1))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:father_id::text, if_defined_or_else(:mother_id?, :mother_id?::text, null), if_defined_or_else(:id?, :id?::text, null)), array(person[id = coalesce(:father_id, if_defined_or_else(:mother_id?, :mother_id, null), if_defined_or_else(:id?, :id?, null))]{id}@(2)), 'Failed to identify value of "father_id" (from optional_params_resolver_test_1) - ' || concat_ws(', ', coalesce(:father_id::text, 'null'), if_defined_or_else(:mother_id?, coalesce(:mother_id?::text, 'null'), '[missing]'), if_defined_or_else(:id?, coalesce(:id?::text, 'null'), '[missing]')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object optional_params_resolver_test_1 {
  def resolve_mother_id(mother_id: Option[java.lang.Long], father_id: java.lang.Long, id: Option[java.lang.Long])(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(if_defined_or_else(:mother_id?, :mother_id::bigint, :father_id::bigint), if_defined_or_else(:id?, :id?::bigint, -1))}"""(env.withParams(Map("father_id" -> father_id) ++ List(Option(mother_id).filter(_.nonEmpty).map(_.get).map(v => "mother_id" -> v), Option(id).filter(_.nonEmpty).map(_.get).map(v => "id" -> v)).filter(_.nonEmpty).map(_.get).toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father_id: java.lang.Long, mother_id: Option[java.lang.Long], id: Option[java.lang.Long])(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:father_id::text, if_defined_or_else(:mother_id?, :mother_id?::text, null), if_defined_or_else(:id?, :id?::text, null)), array(person[id = coalesce(:father_id, if_defined_or_else(:mother_id?, :mother_id, null), if_defined_or_else(:id?, :id?, null))]{id}@(2)), 'Failed to identify value of "father_id" (from optional_params_resolver_test_1) - ' || concat_ws(', ', coalesce(:father_id::text, 'null'), if_defined_or_else(:mother_id?, coalesce(:mother_id?::text, 'null'), '[missing]'), if_defined_or_else(:id?, coalesce(:id?::text, 'null'), '[missing]')))}"""(env.withParams(Map("father_id" -> father_id) ++ List(Option(mother_id).filter(_.nonEmpty).map(_.get).map(v => "mother_id" -> v), Option(id).filter(_.nonEmpty).map(_.get).map(v => "id" -> v)).filter(_.nonEmpty).map(_.get).toMap))
      .unique[java.lang.Long]
  }
}
class organization_account_key_test extends Dto {
  var organization_id: java.lang.Long = null
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class organization_account_key_test_2 extends Dto {
  var balance: scala.math.BigDecimal = null
}
class organization_key_test extends Dto {
  var name: String = null
  var main_account: organization_key_test_main_account = null
  var accounts: List[organization_key_test_accounts] = Nil
}
class organization_key_test_accounts extends Dto {
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class organization_key_test_main_account extends Dto {
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class organization_readonly_children_test extends organization_key_test with Dto {
  var name: String = null
  var main_account: organization_key_test_main_account = null
  // override var accounts: List[organization_key_test_accounts] = Nil
}
class organization_ref_only_update_legacy_test extends Dto {
  var name: String = null
  var main_account: organization_ref_only_update_legacy_test_main_account = null
}
class organization_ref_only_update_legacy_test_main_account extends Dto {
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class organization_ref_only_update_test extends Dto {
  var name: String = null
  var main_account: organization_ref_only_update_test_main_account = null
}
class organization_ref_only_update_test_main_account extends Dto {
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class organization_with_accounts extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var main_account: String = null
  var accounts: List[organization_with_accounts_accounts] = Nil
  def resolve_main_account_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:main_account, array(organization_account[number = :main_account]{id}@(2)), 'Failed to identify value of "main_account" (from organization_with_accounts) - ' || coalesce(:main_account, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object organization_with_accounts {
  def resolve_main_account_id(main_account: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:main_account, array(organization_account[number = :main_account]{id}@(2)), 'Failed to identify value of "main_account" (from organization_with_accounts) - ' || coalesce(:main_account, 'null'))}"""(env.withParams(Map("main_account" -> main_account)))
      .unique[java.lang.Long]
  }
}
class organization_with_accounts_accounts extends DtoWithId {
  var id: java.lang.Long = null
  var number: String = null
  var balance: scala.math.BigDecimal = null
}
class person extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother_id: java.lang.Long = null
  var father_id: java.lang.Long = null
}
class person_2 extends DtoWithId {
  var id: java.lang.Long = null
  var full_name: String = null
  var notes: String = null
}
class person_accounts_2 extends DtoWithId {
  var id: java.lang.Long = null
  var accounts: List[person_accounts_2_accounts] = Nil
}
class person_accounts_2_accounts extends Dto {
  var account_name: String = null
}
class person_and_car_01 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_02 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_03 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_04 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_05 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_06 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_07 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_08 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_09 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_and_car_10 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var car_name: String = null
}
class person_choice_resolver_implied extends Dto {
  var full_name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#2)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_choice_resolver_implied {
  def resolve_id(full_name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#2)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_choice_resolver_provided extends Dto {
  var full_name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#4)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_choice_resolver_provided {
  def resolve_id(full_name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#4)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_choice_without_resolver extends Dto {
  var full_name: String = null
}
class person_from_multiple_db_1 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var other_db_person: person_from_multiple_db_1_other_db_person = null
}
class person_from_multiple_db_1_other_db_person extends Dto {
  var full_name: String = null
}
class person_from_multiple_db_2 extends DtoWithId {
  var id: java.lang.Long = null
  var full_name: String = null
  var other_db_person: person_from_multiple_db_2_other_db_person = null
}
class person_from_multiple_db_2_other_db_person extends Dto {
  var name: String = null
  var surname: String = null
}
class person_info extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother_name: String = null
  var father_name: String = null
  var maternal_grandmother: String = null
  var maternal_grandfather: String = null
  var paternal_grandmother: String = null
  var paternal_grandfather: String = null
  var children: List[person_name] = Nil
  var father: person_info_father = null
}
class person_info_alt extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother_name: String = null
  var father_name: String = null
  var maternal_grandmother: String = null
  var maternal_grandfather: String = null
  var paternal_grandmother: String = null
  var paternal_grandfather: String = null
  var children: List[person_info_alt_children] = Nil
  var father: person_info_alt_father = null
}
class person_info_alt_children extends Dto {
  var name: String = null
}
class person_info_alt_father extends Dto {
  var name: String = null
  var surname: String = null
}
class person_info_father extends Dto {
  var name: String = null
  var surname: String = null
}
class person_multifield_choice_resolver_implied extends Dto {
  var name: String = null
  var full_name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#5)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_multifield_choice_resolver_implied {
  def resolve_id(full_name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#5)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_multifield_choice_resolver_provided extends Dto {
  var name: String = null
  var full_name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#7)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_multifield_choice_resolver_provided {
  def resolve_id(full_name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#7)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_multitable_choice_resolver_implied_1 extends Dto {
  var full_name: String = null
}
class person_multitable_choice_resolver_implied_2 extends Dto {
  var full_name: String = null
}
class person_name extends Dto {
  var name: String = null
}
class person_with_complex_type_resolvers_1 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother: person_with_complex_type_resolvers_1_mother = null
  var father: person_with_complex_type_resolvers_1_father = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:mother.name, :mother.surname), array(person[name = :mother.name & surname = :mother.surname]{id}@(2)), 'Failed to identify value of "mother" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:mother.name, 'null'), coalesce(:mother.surname, 'null')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname), array(person[name = :father.name & surname = :father.surname]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_with_complex_type_resolvers_1 {
  def resolve_mother_id(mother: person_with_complex_type_resolvers_1_mother)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:mother.name, :mother.surname), array(person[name = :mother.name & surname = :mother.surname]{id}@(2)), 'Failed to identify value of "mother" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:mother.name, 'null'), coalesce(:mother.surname, 'null')))}"""(env.withParams(Map("mother" -> Option(mother).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: person_with_complex_type_resolvers_1_father)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname), array(person[name = :father.name & surname = :father.surname]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null')))}"""(env.withParams(Map("father" -> Option(father).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
}
class person_with_complex_type_resolvers_1_father extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_1_father {
  def resolve_sex(sex: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_complex_type_resolvers_1_mother extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:sex, 'F')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_1_mother {
  def resolve_sex(sex: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:sex, 'F')}"""(env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_complex_type_resolvers_2 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var father: person_with_complex_type_resolvers_2_father = null
  def resolve_father_id(`[APPLY OTHER]`: Map[String, Any] => Map[String, Any])(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname, if_defined_or_else(:father.is_resolver_disabled?, :father.is_resolver_disabled?::text, null)), array(person[name = :father.name & surname = :father.surname & !:father.is_resolver_disabled?]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null'), if_defined_or_else(:father.is_resolver_disabled?, coalesce(:father.is_resolver_disabled?::text, 'null'), '[missing]')))}"""(env.withParams(`[APPLY OTHER]`(this.toMap)))
      .unique[java.lang.Long]
  }
}
object person_with_complex_type_resolvers_2 {
  def resolve_father_id(father: person_with_complex_type_resolvers_2_father, `[APPLY OTHER]`: Map[String, Any] => Map[String, Any])(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname, if_defined_or_else(:father.is_resolver_disabled?, :father.is_resolver_disabled?::text, null)), array(person[name = :father.name & surname = :father.surname & !:father.is_resolver_disabled?]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null'), if_defined_or_else(:father.is_resolver_disabled?, coalesce(:father.is_resolver_disabled?::text, 'null'), '[missing]')))}"""(env.withParams(`[APPLY OTHER]`(Map("father" -> Option(father).map(_.toMap).orNull))))
      .unique[java.lang.Long]
  }
}
class person_with_complex_type_resolvers_2_father extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_2_father {
  def resolve_sex(sex: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_parents_1 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother: person_with_parents_1_mother = null
  var father: person_with_parents_1_father = null
}
class person_with_parents_1_father extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_parents_1_father {
  def resolve_sex(sex: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:sex, 'M')}"""(env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_parents_1_mother extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{coalesce(:sex, 'F')}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_parents_1_mother {
  def resolve_sex(sex: String)(implicit env: org.tresql.Resources) = {
    tresql"""{coalesce(:sex, 'F')}"""(env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class ref_expression_test_person_2b extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ref_expression_test_person_2b) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from ref_expression_test_person_2b) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object ref_expression_test_person_2b {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ref_expression_test_person_2b) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from ref_expression_test_person_2b) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class ref_expression_test_person_2c extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
}
class ref_test_bank_2 extends Dto {
  var name: String = null
  var country: String = null
}
class ref_test_bank_3 extends Dto {
  var name: String = null
  var country_c3_and_name: String = null
}
class ref_test_bank_4 extends Dto {
  var name: String = null
  var country_c3_and_name: String = null
}
class ref_test_bank_5 extends Dto {
  var name: String = null
  var country_c3_and_name: String = null
}
class ref_test_bank_6 extends Dto {
  var name: String = null
  var country_c3_and_name: String = null
}
class resolver_alias_clash_test_person_7_a extends Dto {
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_7_a) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_7_a {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_7_a) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_alias_clash_test_person_8_a extends Dto {
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_a) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_8_a {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_a) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_alias_clash_test_person_8_b extends Dto {
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_b) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_8_b {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_b) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_override_test_01 extends resolver_test_person_11 with DtoWithId {
  // override var name: String = null
  // override var id: java.lang.Long = null
  def resolve_name(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{'X' name}@(2)), 'Failed to identify value of "name" (from resolver_override_test_01) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[String]
  }
  override def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(person[name = :name || 'X']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_01) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_override_test_01 {
  def resolve_name(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{'X' name}@(2)), 'Failed to identify value of "name" (from resolver_override_test_01) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[String]
  }
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(person[name = :name || 'X']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_01) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_override_test_02 extends resolver_test_person_11 with DtoWithId {
  var name: String = null
  // override var id: java.lang.Long = null
}
class resolver_override_test_03 extends resolver_override_test_02 with DtoWithId {
  var name: String = null
  // override var id: java.lang.Long = null
  override def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(person[name = :name || 'Y']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_03) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_override_test_03 {
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(person[name = :name || 'Y']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_03) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_override_test_04 extends resolver_override_test_03 with DtoWithId {
  var name: String = null
  // override var id: java.lang.Long = null
  override def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :id::text), array(person[name = :name || :id || 'Z']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_04) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:id::text, 'null')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_override_test_04 {
  def resolve_id(name: String, id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :id::text), array(person[name = :name || :id || 'Z']{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_04) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:id::text, 'null')))}"""(env.withParams(Map("name" -> name, "id" -> id)))
      .unique[java.lang.Long]
  }
}
class resolver_override_test_05 extends resolver_override_test_04 with DtoWithId {
  var name: String = null
  // override var id: java.lang.Long = null
  def resolve_id(unknown_id: java.lang.Long)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :unknown_id::text), array(person[name = :name || :unknown_id]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_05) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:unknown_id::text, 'null')))}"""(env.withParams(this.toMap ++ Map("unknown_id" -> unknown_id)))
      .unique[java.lang.Long]
  }
}
object resolver_override_test_05 {
  def resolve_id(name: String, unknown_id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :unknown_id::text), array(person[name = :name || :unknown_id]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_05) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:unknown_id::text, 'null')))}"""(env.withParams(Map("name" -> name, "unknown_id" -> unknown_id)))
      .unique[java.lang.Long]
  }
}
class resolver_override_test_06 extends resolver_override_test_05 with DtoWithId {
  var name: String = null
  // override var id: java.lang.Long = null
  override def resolve_id(other_unknown_id: java.lang.Long)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :other_unknown_id::text), array(person[name = :name || :other_unknown_id]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_06) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:other_unknown_id::text, 'null')))}"""(env.withParams(this.toMap ++ Map("other_unknown_id" -> other_unknown_id)))
      .unique[java.lang.Long]
  }
}
object resolver_override_test_06 {
  def resolve_id(name: String, other_unknown_id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :other_unknown_id::text), array(person[name = :name || :other_unknown_id]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_override_test_06) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:other_unknown_id::text, 'null')))}"""(env.withParams(Map("name" -> name, "other_unknown_id" -> other_unknown_id)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_1 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  def resolve_bank_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:code, array(bank[code = :code]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_1) - ' || coalesce(:code, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_1 {
  def resolve_bank_id(code: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:code, array(bank[code = :code]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_1) - ' || coalesce(:code, 'null'))}"""(env.withParams(Map("code" -> code)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_2 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  def resolve_bank_id(some_other_variable: String)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:code, :some_other_variable::text), array(bank[code = :code && :some_other_variable]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable::text, 'null')))}"""(env.withParams(this.toMap ++ Map("some_other_variable" -> some_other_variable)))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_2 {
  def resolve_bank_id(code: String, some_other_variable: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:code, :some_other_variable::text), array(bank[code = :code && :some_other_variable]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable::text, 'null')))}"""(env.withParams(Map("code" -> code, "some_other_variable" -> some_other_variable)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_currency_1 extends Dto {
  var account: String = null
  var currency_name: String = null
  def resolve_account_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:account, array(account[billing_account = :account]{id}@(2)), 'Failed to identify value of "account" (from resolver_test_account_currency_1) - ' || coalesce(:account, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_currency_code(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:currency_name, array(currency[name = :currency_name]{code}@(2)), 'Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - ' || coalesce(:currency_name, 'null'))}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_account_currency_1 {
  def resolve_account_id(account: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:account, array(account[billing_account = :account]{id}@(2)), 'Failed to identify value of "account" (from resolver_test_account_currency_1) - ' || coalesce(:account, 'null'))}"""(env.withParams(Map("account" -> account)))
      .unique[java.lang.Long]
  }
  def resolve_currency_code(currency_name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:currency_name, array(currency[name = :currency_name]{code}@(2)), 'Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - ' || coalesce(:currency_name, 'null'))}"""(env.withParams(Map("currency_name" -> currency_name)))
      .unique[String]
  }
}
class resolver_test_account_self_ref_1 extends Dto {
  var name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_1) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_self_ref_1 {
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_1) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_self_ref_2 extends Dto {
  var name: String = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_2) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_self_ref_2 {
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_2) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_bank_1 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  def resolve_name(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{'My bank'}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_bank_1 {
  def resolve_name(implicit env: org.tresql.Resources) = {
    tresql"""{'My bank'}"""(env.withParams(Map.empty))
      .unique[String]
  }
}
class resolver_test_bank_2 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  def resolve_name(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:name || ' saved'}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_bank_2 {
  def resolve_name(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{:name || ' saved'}"""(env.withParams(Map("name" -> name)))
      .unique[String]
  }
}
class resolver_test_person_1 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person[name || surname = :mother]{id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name || surname = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_1) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_1 {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person[name || surname = :mother]{id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name || surname = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_1) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_10 extends DtoWithId {
  var name: String = null
  var id: java.lang.Long = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{id}@(2)), 'Failed to identify value of "id" (from resolver_test_person_10) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_10 {
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{id}@(2)), 'Failed to identify value of "id" (from resolver_test_person_10) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_11 extends DtoWithId {
  var name: String = null
  var id: java.lang.Long = null
  def resolve_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_test_person_11) - ' || coalesce(:name, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_11 {
  def resolve_id(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:name, array(person[name = :name]{nullif(0, 0) id}@(2)), 'Failed to identify value of "id" (from resolver_test_person_11) - ' || coalesce(:name, 'null'))}"""(env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_12_a extends DtoWithId {
  var id: java.lang.Long = null
  var father: String = null
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name = :father & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_a) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_12_a {
  def resolve_father_id(father: String, id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name = :father & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_a) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father, "id" -> id)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_12_b extends DtoWithId {
  var id: java.lang.Long = null
  var father: String = null
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name = :father & :id > 0 & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_b) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_12_b {
  def resolve_father_id(father: String, id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name = :father & :id > 0 & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_b) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father, "id" -> id)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_12_c extends DtoWithId {
  var id: java.lang.Long = null
  var father: String = null
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:father, :id::text), array(person[name = :father & :id > 0 & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_c) - ' || concat_ws(', ', coalesce(:father, 'null'), coalesce(:id::text, 'null')))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_12_c {
  def resolve_father_id(father: String, id: java.lang.Long)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:father, :id::text), array(person[name = :father & :id > 0 & id > :id]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_12_c) - ' || concat_ws(', ', coalesce(:father, 'null'), coalesce(:id::text, 'null')))}"""(env.withParams(Map("father" -> father, "id" -> id)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_2 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  var empty_expression_test: String = null
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_2) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_2 {
  def resolve_father_id(father: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_2) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_3 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#2)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_3) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#4)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_3) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_3 {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#2)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_3) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#4)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_3) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_4 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{1}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{2}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_4 {
  def resolve_mother_id(implicit env: org.tresql.Resources) = {
    tresql"""{1}"""(env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources) = {
    tresql"""{2}"""(env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_5 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#5)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_5) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#7)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_5) - ' || coalesce(:father, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_5 {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#5)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_5) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#7)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_5) - ' || coalesce(:father, 'null'))}"""(env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_6 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{3}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{4}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_6 {
  def resolve_mother_id(implicit env: org.tresql.Resources) = {
    tresql"""{3}"""(env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
  def resolve_father_id(implicit env: org.tresql.Resources) = {
    tresql"""{4}"""(env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_7 extends Dto {
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_7) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_7 {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_7) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_8 extends Dto {
  var mother: String = null
  def resolve_mother_id(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_8) - ' || coalesce(:mother, 'null'))}"""(env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_8 {
  def resolve_mother_id(mother: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_8) - ' || coalesce(:mother, 'null'))}"""(env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_9_a extends Dto {
  var name: String = null
  var mother_id: java.lang.Long = null
  def resolve_mother_id(surname: String, `type`: String)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :type::text), array(person[name = :name & surname = :surname & :type = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_a) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:type::text, 'null')))}"""(env.withParams(this.toMap ++ Map("surname" -> surname, "type" -> `type`)))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_9_a {
  def resolve_mother_id(name: String, surname: String, `type`: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :type::text), array(person[name = :name & surname = :surname & :type = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_a) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:type::text, 'null')))}"""(env.withParams(Map("name" -> name, "surname" -> surname, "type" -> `type`)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_9_b extends Dto {
  var name: String = null
  var mother_id: java.lang.Long = null
  def resolve_mother_id(surname: String, `creative param name`: String)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :'creative param name'::text), array(person[name = :name & surname = :surname & :'creative param name' = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_b) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:'creative param name'::text, 'null')))}"""(env.withParams(this.toMap ++ Map("surname" -> surname, "creative param name" -> `creative param name`)))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_9_b {
  def resolve_mother_id(name: String, surname: String, `creative param name`: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :'creative param name'::text), array(person[name = :name & surname = :surname & :'creative param name' = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_b) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:'creative param name'::text, 'null')))}"""(env.withParams(Map("name" -> name, "surname" -> surname, "creative param name" -> `creative param name`)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_9_c extends Dto {
  var name: String = null
  var mother_id: java.lang.Long = null
  def resolve_mother_id(surname: String, `creative.param.name`: String)(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :'creative.param.name'::text), array(person[name = :name & surname = :surname & :'creative.param.name' = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_c) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:'creative.param.name'::text, 'null')))}"""(env.withParams(this.toMap ++ Map("surname" -> surname, "creative.param.name" -> `creative.param.name`)))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_9_c {
  def resolve_mother_id(name: String, surname: String, `creative.param.name`: String)(implicit env: org.tresql.Resources) = {
    tresql"""{checked_resolve(coalesce(:name, :surname::text, :'creative.param.name'::text), array(person[name = :name & surname = :surname & :'creative.param.name' = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9_c) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname::text, 'null'), coalesce(:'creative.param.name'::text, 'null')))}"""(env.withParams(Map("name" -> name, "surname" -> surname, "creative.param.name" -> `creative.param.name`)))
      .unique[java.lang.Long]
  }
}
class resolver_test_scala_escapes_01 extends Dto {
  var name: String = null
  def resolve_name(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{case(:name `~` '^\\d+$$', 'dig-only - ' || :name, 'not-dig-only - ' || :name)}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_scala_escapes_01 {
  def resolve_name(name: String)(implicit env: org.tresql.Resources) = {
    tresql"""{case(:name `~` '^\\d+$$', 'dig-only - ' || :name, 'not-dig-only - ' || :name)}"""(env.withParams(Map("name" -> name)))
      .unique[String]
  }
}
class ro_child_ref_clash_test extends DtoWithId {
  var id: java.lang.Long = null
  var person: person = null
  var person_id: java.lang.Long = null
}
class save_extra_props_test_01 extends DtoWithId {
  var id: java.lang.Long = null
}
class save_to_multi_test_01 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var sex: String = null
  var password: String = null
  def resolve_sex(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{'M'}"""(env.withParams(this.toMap))
      .unique[String]
  }
}
object save_to_multi_test_01 {
  def resolve_sex(implicit env: org.tresql.Resources) = {
    tresql"""{'M'}"""(env.withParams(Map.empty))
      .unique[String]
  }
}
class self_ref_test_account_1 extends Dto {
  var full_name: String = null
}
class self_ref_test_account_2 extends Dto {
  var full_name: String = null
}
class siblings extends Dto {
  var sibling1: String = null
  var sibling2: String = null
}
class siblings_alt extends Dto {
  var sibling1: String = null
  var sibling2: String = null
}
class types_test extends DtoWithId {
  var id: java.lang.Long = null
  var long: java.lang.Long = null
  var string: String = null
  var date: java.sql.Date = null
  var time: java.sql.Time = null
  var date_time: java.sql.Timestamp = null
  var int: java.lang.Integer = null
  var bigint: scala.math.BigInt = null
  var double: java.lang.Double = null
  var decimal: scala.math.BigDecimal = null
  var boolean: java.lang.Boolean = null
  var bytes: Array[Byte] = null
  var child: types_test_child = null
  var children: List[types_test_child] = Nil
}
class types_test_child extends Dto {
  var name: String = null
  var date: java.sql.Date = null
  var date_time: java.sql.Timestamp = null
}
class types_test_small extends DtoWithId {
  var id: java.lang.Long = null
  var long: java.lang.Long = null
}
class types_test_small_fake extends DtoWithId {
  var id: java.lang.Long = null
  var fake: String = null
}
class validations_test extends DtoWithId {
  var id: java.lang.Long = null
  var integer_column: java.lang.Integer = null
  var children1: List[validations_test_child_1] = Nil
  var children2: List[validations_test_child_2] = Nil
  def resolve_int_col(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:integer_column}"""(env.withParams(this.toMap))
      .unique[java.lang.Integer]
  }
}
object validations_test {
  def resolve_int_col(integer_column: java.lang.Integer)(implicit env: org.tresql.Resources) = {
    tresql"""{:integer_column}"""(env.withParams(Map("integer_column" -> integer_column)))
      .unique[java.lang.Integer]
  }
}
class validations_test_child extends DtoWithId {
  var id: java.lang.Long = null
  var integer_column: java.lang.Integer = null
  def resolve_int_col(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:integer_column}"""(env.withParams(this.toMap))
      .unique[java.lang.Integer]
  }
}
object validations_test_child {
  def resolve_int_col(integer_column: java.lang.Integer)(implicit env: org.tresql.Resources) = {
    tresql"""{:integer_column}"""(env.withParams(Map("integer_column" -> integer_column)))
      .unique[java.lang.Integer]
  }
}
class validations_test_child_1 extends validations_test_child with DtoWithId {
  var id: java.lang.Long = null
  var integer_column: java.lang.Integer = null
  def resolve_int_col(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:integer_column}"""(env.withParams(this.toMap))
      .unique[java.lang.Integer]
  }
}
object validations_test_child_1 {
  def resolve_int_col(integer_column: java.lang.Integer)(implicit env: org.tresql.Resources) = {
    tresql"""{:integer_column}"""(env.withParams(Map("integer_column" -> integer_column)))
      .unique[java.lang.Integer]
  }
}
class validations_test_child_2 extends validations_test_child with DtoWithId {
  var id: java.lang.Long = null
  var integer_column: java.lang.Integer = null
  def resolve_int_col(implicit env: org.tresql.Resources, qe: QE) = {
    tresql"""{:integer_column}"""(env.withParams(this.toMap))
      .unique[java.lang.Integer]
  }
}
object validations_test_child_2 {
  def resolve_int_col(integer_column: java.lang.Integer)(implicit env: org.tresql.Resources) = {
    tresql"""{:integer_column}"""(env.withParams(Map("integer_column" -> integer_column)))
      .unique[java.lang.Integer]
  }
}
class with_forefathers extends Dto {
  var full_name: String = null
  var father_id: java.lang.Long = null
  var forefathers: List[with_forefathers_forefathers] = Nil
}
class with_forefathers_forefathers extends Dto {
  var full_name: String = null
}
