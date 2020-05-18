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
  def resolve_bank_id = {
    tresql"""{:bank.id}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object account_with_bank {
  def resolve_bank_id(bank: account_with_bank_bank) = {
    tresql"""{:bank.id}"""(Env.withParams(Map("bank" -> Option(bank).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
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
  def resolve_father_id = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ambiguity_resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_mother_id = {
    tresql"""{checked_resolve(:ma, array(person[name || ' ' || surname || ' (#1)' = :ma]{person.id}@(2)), 'Failed to identify value of "ma" (from ambiguity_resolver_test_person_1) - ' || coalesce(:ma, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object ambiguity_resolver_test_person_1 {
  def resolve_father_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ambiguity_resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_mother_id(ma: String) = {
    tresql"""{checked_resolve(:ma, array(person[name || ' ' || surname || ' (#1)' = :ma]{person.id}@(2)), 'Failed to identify value of "ma" (from ambiguity_resolver_test_person_1) - ' || coalesce(:ma, 'null'))}"""(Env.withParams(Map("ma" -> ma)))
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
  def resolve_name_eng = {
    tresql"""{coalesce(:name_eng, '')}"""(Env.withParams(this.toMap))
      .unique[String]
  }
  def resolve_name_rus = {
    tresql"""{coalesce(:name_rus, '')}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object currency {
  def resolve_name_eng(name_eng: String) = {
    tresql"""{coalesce(:name_eng, '')}"""(Env.withParams(Map("name_eng" -> name_eng)))
      .unique[String]
  }
  def resolve_name_rus(name_rus: String) = {
    tresql"""{coalesce(:name_rus, '')}"""(Env.withParams(Map("name_rus" -> name_rus)))
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
class nested_resolver_test_1 extends Dto {
  var other_field: String = null
  var mother: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother & person.father_id = checked_resolve(:other_field, array(person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2)), 'Failed to identify value of "other_field" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field, 'null'))]]{person.id}@(2)), 'Failed to identify value of "mother" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object nested_resolver_test_1 {
  def resolve_mother_id(mother: String, other_field: String) = {
    tresql"""{checked_resolve(coalesce(:mother, :other_field), array(person;person[person.father_id]person? father[[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother & person.father_id = checked_resolve(:other_field, array(person p1;p1[p1.father_id]person? father[[:other_field = p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)']]{p1.id}@(2)), 'Failed to identify value of "other_field" (from person_multitable_choice_resolver_implied_1) - ' || coalesce(:other_field, 'null'))]]{person.id}@(2)), 'Failed to identify value of "mother" (from nested_resolver_test_1) - ' || concat_ws(', ', coalesce(:mother, 'null'), coalesce(:other_field, 'null')))}"""(Env.withParams(Map("mother" -> mother, "other_field" -> other_field)))
      .unique[java.lang.Long]
  }
}
class person extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var mother_id: java.lang.Long = null
  var father_id: java.lang.Long = null
}
class person_choice_resolver_implied extends Dto {
  var full_name: String = null
  def resolve_id = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#2)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_choice_resolver_implied {
  def resolve_id(full_name: String) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#2)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_choice_resolver_provided extends Dto {
  var full_name: String = null
  def resolve_id = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#4)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_choice_resolver_provided {
  def resolve_id(full_name: String) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#4)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_choice_without_resolver extends Dto {
  var full_name: String = null
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
  def resolve_id = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#5)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_multifield_choice_resolver_implied {
  def resolve_id(full_name: String) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#5)' = :full_name]{person.id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_implied) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(Map("full_name" -> full_name)))
      .unique[java.lang.Long]
  }
}
class person_multifield_choice_resolver_provided extends Dto {
  var name: String = null
  var full_name: String = null
  def resolve_id = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#7)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_multifield_choice_resolver_provided {
  def resolve_id(full_name: String) = {
    tresql"""{checked_resolve(:full_name, array(person[name || ' ' || surname || ' (#7)' = :full_name]{id}@(2)), 'Failed to identify value of "full_name" (from person_multifield_choice_resolver_provided) - ' || coalesce(:full_name, 'null'))}"""(Env.withParams(Map("full_name" -> full_name)))
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
  def resolve_mother_id = {
    tresql"""{checked_resolve(coalesce(:mother.name, :mother.surname), array(person[name = :mother.name & surname = :mother.surname]{id}@(2)), 'Failed to identify value of "mother" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:mother.name, 'null'), coalesce(:mother.surname, 'null')))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname), array(person[name = :father.name & surname = :father.surname]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null')))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object person_with_complex_type_resolvers_1 {
  def resolve_mother_id(mother: person_with_complex_type_resolvers_1_mother) = {
    tresql"""{checked_resolve(coalesce(:mother.name, :mother.surname), array(person[name = :mother.name & surname = :mother.surname]{id}@(2)), 'Failed to identify value of "mother" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:mother.name, 'null'), coalesce(:mother.surname, 'null')))}"""(Env.withParams(Map("mother" -> Option(mother).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: person_with_complex_type_resolvers_1_father) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname), array(person[name = :father.name & surname = :father.surname]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_1) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null')))}"""(Env.withParams(Map("father" -> Option(father).map(_.toMap).orNull)))
      .unique[java.lang.Long]
  }
}
class person_with_complex_type_resolvers_1_father extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex = {
    tresql"""{coalesce(:sex, 'M')}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_1_father {
  def resolve_sex(sex: String) = {
    tresql"""{coalesce(:sex, 'M')}"""(Env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_complex_type_resolvers_1_mother extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex = {
    tresql"""{coalesce(:sex, 'F')}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_1_mother {
  def resolve_sex(sex: String) = {
    tresql"""{coalesce(:sex, 'F')}"""(Env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class person_with_complex_type_resolvers_2 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var sex: String = null
  var father: person_with_complex_type_resolvers_2_father = null
  def resolve_father_id(`father.is_resolver_disabled`: java.lang.Boolean) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname, :father.is_resolver_disabled?), array(person[name = :father.name & surname = :father.surname & !:father.is_resolver_disabled?]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null'), coalesce(:father.is_resolver_disabled?, 'null')))}"""(Env.withParams(this.toMap ++ Map("father.is_resolver_disabled" -> `father.is_resolver_disabled`)))
      .unique[java.lang.Long]
  }
}
object person_with_complex_type_resolvers_2 {
  def resolve_father_id(father: person_with_complex_type_resolvers_2_father, `father.is_resolver_disabled`: java.lang.Boolean) = {
    tresql"""{checked_resolve(coalesce(:father.name, :father.surname, :father.is_resolver_disabled?), array(person[name = :father.name & surname = :father.surname & !:father.is_resolver_disabled?]{id}@(2)), 'Failed to identify value of "father" (from person_with_complex_type_resolvers_2) - ' || concat_ws(', ', coalesce(:father.name, 'null'), coalesce(:father.surname, 'null'), coalesce(:father.is_resolver_disabled?, 'null')))}"""(Env.withParams(Map("father" -> Option(father).map(_.toMap).orNull, "father.is_resolver_disabled" -> `father.is_resolver_disabled`)))
      .unique[java.lang.Long]
  }
}
class person_with_complex_type_resolvers_2_father extends Dto {
  var name: String = null
  var surname: String = null
  var sex: String = null
  def resolve_sex = {
    tresql"""{coalesce(:sex, 'M')}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object person_with_complex_type_resolvers_2_father {
  def resolve_sex(sex: String) = {
    tresql"""{coalesce(:sex, 'M')}"""(Env.withParams(Map("sex" -> sex)))
      .unique[String]
  }
}
class ref_expression_test_person_2b extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ref_expression_test_person_2b) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from ref_expression_test_person_2b) - ' || coalesce(:father, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object ref_expression_test_person_2b {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#1)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from ref_expression_test_person_2b) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from ref_expression_test_person_2b) - ' || coalesce(:father, 'null'))}"""(Env.withParams(Map("father" -> father)))
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
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_7_a) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_7_a {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_7_a) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_alias_clash_test_person_8_a extends Dto {
  var mother: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_a) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_8_a {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_a) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_alias_clash_test_person_8_b extends Dto {
  var mother: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_b) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_alias_clash_test_person_8_b {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_alias_clash_test_person_8_b) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_1 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  def resolve_bank_id = {
    tresql"""{checked_resolve(:code, array(bank[code = :code]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_1) - ' || coalesce(:code, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_1 {
  def resolve_bank_id(code: String) = {
    tresql"""{checked_resolve(:code, array(bank[code = :code]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_1) - ' || coalesce(:code, 'null'))}"""(Env.withParams(Map("code" -> code)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_2 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  def resolve_bank_id(some_other_variable: String) = {
    tresql"""{checked_resolve(coalesce(:code, :some_other_variable), array(bank[code = :code && :some_other_variable]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable, 'null')))}"""(Env.withParams(this.toMap ++ Map("some_other_variable" -> some_other_variable)))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_2 {
  def resolve_bank_id(code: String, some_other_variable: String) = {
    tresql"""{checked_resolve(coalesce(:code, :some_other_variable), array(bank[code = :code && :some_other_variable]{id}@(2)), 'Failed to identify value of "code" (from resolver_test_account_2) - ' || concat_ws(', ', coalesce(:code, 'null'), coalesce(:some_other_variable, 'null')))}"""(Env.withParams(Map("code" -> code, "some_other_variable" -> some_other_variable)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_currency_1 extends Dto {
  var account: String = null
  var currency_name: String = null
  def resolve_account_id = {
    tresql"""{checked_resolve(:account, array(account[billing_account = :account]{id}@(2)), 'Failed to identify value of "account" (from resolver_test_account_currency_1) - ' || coalesce(:account, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_currency_code = {
    tresql"""{checked_resolve(:currency_name, array(currency[name = :currency_name]{code}@(2)), 'Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - ' || coalesce(:currency_name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_account_currency_1 {
  def resolve_account_id(account: String) = {
    tresql"""{checked_resolve(:account, array(account[billing_account = :account]{id}@(2)), 'Failed to identify value of "account" (from resolver_test_account_currency_1) - ' || coalesce(:account, 'null'))}"""(Env.withParams(Map("account" -> account)))
      .unique[java.lang.Long]
  }
  def resolve_currency_code(currency_name: String) = {
    tresql"""{checked_resolve(:currency_name, array(currency[name = :currency_name]{code}@(2)), 'Failed to identify value of "currency_name" (from resolver_test_account_currency_1) - ' || coalesce(:currency_name, 'null'))}"""(Env.withParams(Map("currency_name" -> currency_name)))
      .unique[String]
  }
}
class resolver_test_account_self_ref_1 extends Dto {
  var name: String = null
  def resolve_id = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_1) - ' || coalesce(:name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_self_ref_1 {
  def resolve_id(name: String) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_1) - ' || coalesce(:name, 'null'))}"""(Env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_account_self_ref_2 extends Dto {
  var name: String = null
  def resolve_id = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_2) - ' || coalesce(:name, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_account_self_ref_2 {
  def resolve_id(name: String) = {
    tresql"""{checked_resolve(:name, array(account;account/bank?[bank.code || ', ' || bank.name || ', ' || account.id = :name]{account.id}@(2)), 'Failed to identify value of "name" (from resolver_test_account_self_ref_2) - ' || coalesce(:name, 'null'))}"""(Env.withParams(Map("name" -> name)))
      .unique[java.lang.Long]
  }
}
class resolver_test_bank_1 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  def resolve_name = {
    tresql"""{'My bank'}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_bank_1 {
  def resolve_name = {
    tresql"""{'My bank'}"""(Env.withParams(Map.empty))
      .unique[String]
  }
}
class resolver_test_bank_2 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  def resolve_name = {
    tresql"""{:name || ' saved'}"""(Env.withParams(this.toMap))
      .unique[String]
  }
}
object resolver_test_bank_2 {
  def resolve_name(name: String) = {
    tresql"""{:name || ' saved'}"""(Env.withParams(Map("name" -> name)))
      .unique[String]
  }
}
class resolver_test_person_1 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person[name || surname = :mother]{id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{checked_resolve(:father, array(person[name || surname = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_1) - ' || coalesce(:father, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_1 {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person[name || surname = :mother]{id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_1) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String) = {
    tresql"""{checked_resolve(:father, array(person[name || surname = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_1) - ' || coalesce(:father, 'null'))}"""(Env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_2 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  var empty_expression_test: String = null
  def resolve_father_id = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_2) - ' || coalesce(:father, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_2 {
  def resolve_father_id(father: String) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#1)' = :father]{person.id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_2) - ' || coalesce(:father, 'null'))}"""(Env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_3 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#2)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_3) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#4)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_3) - ' || coalesce(:father, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_3 {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#2)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_3) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#4)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_3) - ' || coalesce(:father, 'null'))}"""(Env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_4 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{1}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{2}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_4 {
  def resolve_mother_id = {
    tresql"""{1}"""(Env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{2}"""(Env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_5 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#5)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_5) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#7)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_5) - ' || coalesce(:father, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_5 {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person[name || ' ' || surname || ' (#5)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_5) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
  def resolve_father_id(father: String) = {
    tresql"""{checked_resolve(:father, array(person[name || ' ' || surname || ' (#7)' = :father]{id}@(2)), 'Failed to identify value of "father" (from resolver_test_person_5) - ' || coalesce(:father, 'null'))}"""(Env.withParams(Map("father" -> father)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_6 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
  def resolve_mother_id = {
    tresql"""{3}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{4}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_6 {
  def resolve_mother_id = {
    tresql"""{3}"""(Env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
  def resolve_father_id = {
    tresql"""{4}"""(Env.withParams(Map.empty))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_7 extends Dto {
  var mother: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_7) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_7 {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person;person[person.father_id]person? father[person.name || ' ' || person.surname || ' of ' || father.name || ' (#7)' = :mother]{person.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_7) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_8 extends Dto {
  var mother: String = null
  def resolve_mother_id = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_8) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(this.toMap))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_8 {
  def resolve_mother_id(mother: String) = {
    tresql"""{checked_resolve(:mother, array(person p1;p1[p1.father_id]person? father[p1.name || ' ' || p1.surname || ' of ' || father.name || ' (#8)' = :mother]{p1.id}@(2)), 'Failed to identify value of "mother" (from resolver_test_person_8) - ' || coalesce(:mother, 'null'))}"""(Env.withParams(Map("mother" -> mother)))
      .unique[java.lang.Long]
  }
}
class resolver_test_person_9 extends Dto {
  var name: String = null
  var mother_id: java.lang.Long = null
  def resolve_mother_id(surname: String, `type`: String) = {
    tresql"""{checked_resolve(coalesce(:name, :surname, :type), array(person[name = :name & surname = :surname & :type = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname, 'null'), coalesce(:type, 'null')))}"""(Env.withParams(this.toMap ++ Map("surname" -> surname, "type" -> `type`)))
      .unique[java.lang.Long]
  }
}
object resolver_test_person_9 {
  def resolve_mother_id(name: String, surname: String, `type`: String) = {
    tresql"""{checked_resolve(coalesce(:name, :surname, :type), array(person[name = :name & surname = :surname & :type = 'person']{id}@(2)), 'Failed to identify value of "mother_id" (from resolver_test_person_9) - ' || concat_ws(', ', coalesce(:name, 'null'), coalesce(:surname, 'null'), coalesce(:type, 'null')))}"""(Env.withParams(Map("name" -> name, "surname" -> surname, "type" -> `type`)))
      .unique[java.lang.Long]
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
class with_forefathers extends Dto {
  var full_name: String = null
  var father_id: java.lang.Long = null
  var forefathers: List[with_forefathers_forefathers] = Nil
}
class with_forefathers_forefathers extends Dto {
  var full_name: String = null
}
