# querease

[![Latest version](https://img.shields.io/maven-central/v/org.mojoz/querease_3)](https://central.sonatype.com/artifact/org.mojoz/querease_3)
![Build Status](https://github.com/guntiso/querease/actions/workflows/ci.yaml/badge.svg)

Queries and persists data from [mojoz](https://github.com/guntiso/mojoz) view metadata using [tresql](https://github.com/mrumkovskis/tresql).
Generates Scala DTO classes from views.
Used by [wabase](https://github.com/mrumkovskis/wabase) to save and retrieve data.

Table and view YAML is documented in [mojoz](https://github.com/guntiso/mojoz).
Querease loads it from `tables` and `views` classpath resources by default
(see [YamlMd](https://static.javadoc.io/org.mojoz/mojoz_3/7.2.0/org/mojoz/metadata/in/YamlMd$.html).fromPaths)
and turns each view into a tresql query or a persistence plan.

Add to `build.sbt`:
```
libraryDependencies += "org.mojoz" %% "querease" % "<version>"
```

## Querying

[Querease](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/Querease.html)
builds a [tresql](https://github.com/mrumkovskis/tresql/blob/master/docs/language-guide.md) query from a view,
binds parameters, and maps rows to DTOs (or maps). Methods take an implicit
[Resources](https://static.javadoc.io/org.tresql/tresql_3/13.5.1/org/tresql/Resources.html)
(JDBC connection, dialect, metadata, macros) and a
[QuereaseIo](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseIo.html)
that converts rows to DTOs.

The DTO simple class name is the view name unless `viewNameFromMf` is overridden.
[ScalaDtoGenerator](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ScalaDtoGenerator.html)
emits classes named after views by default; [sbt-mojoz](https://github.com/guntiso/sbt-mojoz)
typically capitalizes them and maps names back.

For example, given
```yaml
name: bank_list_row
table: bank
fields:
- id
- code
- name
order: ~id
```
```scala
implicit val qe: Querease = new Querease
implicit val qio: QuereaseIo[Dto] = new ScalaDtoQuereaseIo[Dto](qe)
implicit val resources: Resources = ... // tresql Resources with connection and metadata

val banks = qe.list[bank_list_row](Map.empty, orderBy = "id")
val bank  = qe.get[bank_list_row](10000L)
val n     = qe.countAll[bank_list_row](Map.empty)
```
`queryStringAndParams` produces the tresql string and bind map, here
`bank {bank.id, bank.code, bank.name}#(~id)`. Paths such as `country.name` add a join
(or a subquery for `^view.field` and for child views).

Methods:

* **get** — unique optional row by key. `get(id: Long)` uses a long key column (typically `id`);
  `get(code: String)` uses a string key column (typically `code`);
  `get(keyValues: Seq[Any])` uses the view [key](#key) columns.
  Extra filter, extra params and [FieldFilter](#optional-fields) can be passed.
* **list** — all matching rows as `List`. Honours `offset`, `limit`, `orderBy`,
  view `filter` plus `extraFilter`, extra params and FieldFilter.
* **result** — same as list, as a lazy auto-closing
  [QuereaseIteratorResult](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseIteratorResult.html).
* **countAll** — `count(*)` for the same filters (without limit/offset).
* **create** — new instance. If any field has [initial](#initial-values), those expressions are evaluated;
  otherwise a no-arg constructor is used.

`list` / `result` / `countAll` bind `params` to view filters. `orderBy` overrides view `order`
(`~` in tresql is descending). `offset` / `limit` become tresql `@(? ?)`.
`extraFilter` is an additional tresql where fragment.

Child views become nested tresql selects. Refs such as `mother.name` follow foreign keys.
A recursive child (same view name) is emitted as a tresql `|` subselect
(see `father_tree` in [person.yaml](test/views/person.yaml)).
Joins, filter, group, having, distinct and order from the view are included as documented in
[mojoz](https://github.com/guntiso/mojoz#extends-joins-and-query-clauses).

### Filters

View **filter** is a list of where-clause fragments (typically tresql).
Querease expands identifier-style filters to bind variables
(see [FilterTransformer](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/FilterTransformer.html)):

| Filter | Becomes |
| --- | --- |
| `name` | `name = :name?` (optional bind) |
| `name!` | `name = :name` (required bind) |
| `name >` | `name > :name?` |
| `name ~` | `name ~ :name?` |
| `< last_modified <` | `:last_modified_from? < last_modified < :last_modified_to?` |
| `! < last_modified < !` | `:last_modified_from < last_modified < :last_modified_to` |
| `^full_name` | field expression `= :full_name?` |
| `true` / `false` | left as is |
| other tresql | left as is |

Unqualified identifiers are prefixed with the base table alias.
`x_.col` binds as `:col`; `a.col` binds as `:a_col`.
`^view.field` in a filter or field expression is replaced with that field's query expression
(see `ref_test_bank_2` in [bank.yaml](test/views/bank.yaml)).
A trailing `!` on a comparison or interval bound makes the corresponding bind required.

```yaml
name: bank_list_row_with_filter
table: bank
fields:
- id
- code
- name
filter: code != "b2"
```

```yaml
name: resolver_test_person_10
table: person p10
fields:
- name
- id -> = person[name = :name]{id}
filter:
- name
```

The second view lists as `person p10[p10.name = :name?] {p10.name, ...}` —
pass `Map("name" -> "Ann")` to filter.

### Optional fields

Field option `?` marks a field optional
(see [mojoz field options](https://github.com/guntiso/mojoz#fields)).
[ScalaDtoGenerator](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ScalaDtoGenerator.html)
emits `Option`. Missing / `None` is omitted from maps and from save (the column is not updated).
[FieldFilter](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/FieldFilter.html)
can exclude optional fields from a query:

```yaml
name:   organization_account_optional_fields_test
table:  organization_account
fields:
- id
- number        [?]
- balance       [?]
- organization  [?]:
    table: organization
    fields:
    - name
```

```scala
val noOptionalFields: FieldFilter = new FieldFilter {
  def shouldInclude(field: String) = false
  def childFilter(field: String) = this
}
qe.get[organization_account_optional_fields_test](id, noOptionalFields)
```

## Persistence

Save, insert, update, upsert and delete use tresql
[ORT](https://github.com/mrumkovskis/tresql) (object-relational transformation).
Validation runs before save. Target table is **save-to** if set, otherwise the view **table**.

```scala
val bank = new bank_list_row
bank.code = "b1"
bank.name = "Bank 1"
val id = qe.save(bank)          // insert if key is missing, otherwise update; returns id as Long
qe.save(bank, forceInsert = true)
qe.delete(bank)
```

Methods:

* **save**(pojo) — insert if all [key](#key) field values are missing, otherwise update.
  `forceInsert = true` always inserts. Optional `extraPropsToSave`, extra `filter` and `params`.
* **save**(view, data, method, filter) — `SaveMethod.Save` / `Insert` / `Update` / `Upsert`.
  Returns `(methodUsed, id)`.
* **validateAndSave** — validate, then save.
* **insert** / **update** / **upsert** — by view and `Map`.
  Upsert returns `(Insert | Update, id)`.
* **delete**(instance) or **delete**(view, data, filter, params) — by key.
  Returns deleted row count.

Zero rows inserted, updated, upserted or deleted throw
[NotFoundException](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/NotFoundException.html).
Failed [validations](#validations) throw
[ValidationException](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ValidationException.html).

If persistence options are omitted, simple fields are inserted and updated;
children are inserted and deleted, not updated, unless `=` is set
(see [mojoz nested views](https://github.com/guntiso/mojoz#nested-views)).

```yaml
name:   bank_with_accounts_1
table:  bank
fields:
- id
- code
- name
- accounts[+-=] * :          # insert, update and delete children
    table: account
    fields:
    - id
    - billing_account
    - last_modified -> = now()
```

```yaml
name: organization
table: organization
fields:
- name
- main_account [/!]:         # update the ref, do not save the child
    table: organization_account
    fields:
    - number
    - balance
```

`+` insert, `=` update, `-` delete (children), `!` read-only.
`/` separates options for this field from options for a referenced child.

To persist to several tables:
```yaml
name:     organization_with_facts
table:    organization o
save-to:
- organization
- organization_facts
joins:
- o/[o.id = f_.id]organization_facts f_
fields:
- id
- name
- actual_address  [a.id = f_.actual_address_id] address
- legal_address   [a.id = o.legal_address_id]   address
```

Empty **save-to** on a child means the child is not persisted as its own row.
`save-to: person:mother_id` on a child resolves an ambiguous parent ref.

An existing row can be identified by a previous key when the key itself is being changed:
pass `"old key" -> Map(fieldName -> oldValue)` in the save data
(see `oldKeyParamName` in
[QuereaseMetadata](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseMetadata.html)).

### Key

Used by **get**, **save** (insert vs update), **delete**, and by
[wabase](https://github.com/mrumkovskis/wabase) to parse URI keys.

If **key** is omitted, the table primary key or a unique key is used when all of its columns
are present as view fields.

```yaml
name:    person
table:   person
key:     code
fields:
- code
- name
- surname
```

```yaml
name: fake_key_test
key: id, sha_256
fields:
```

```yaml
name: typed_fake_key_test
key: id::string, sha_256
fields:
```

```yaml
name: field_like_key_test
key:
- id                   : Identifier
- choice 2  (a, bb, cc): Enum in key
fields:
```

`key: a, b(, c, d)` — all of `a, b, c, d` are key fields; fields before `(`
(`a, b`) are the minimum search key (`minSearchKeyFieldCount`), used by wabase
to distinguish collection requests from get-by-key.

### Resolvers

`->` on a field converts the field value to a column value when saving
(see [mojoz expressions and resolvers](https://github.com/guntiso/mojoz#expressions-and-resolvers)).
Querease wraps implied resolvers in `checked_resolve` so a missing or ambiguous match
fails instead of silently writing null.

```yaml
name:  resolver_test_person_1
table: person
fields:
- id
- mother = mother.name || mother.surname -> mother_id
- father = father.name || father.surname -> father_id = person[name || surname = _]{id}
```

```yaml
name: resolver_test_account_1
table: account
fields:
- id
- code = -> bank_id
```

`code = -> bank_id` is implied from the `bank_id` ref: `bank[code = _]{id}`.
An explicit resolver uses `_` for the field value. `^other_view.field` reuses that field's
expression and resolver.

On PostgreSQL, define `checked_resolve` so a non-unique or missing match raises an error
(see `resolverExpression` in
[QuereaseExpressions](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseExpressions.html)):
```sql
create or replace function checked_resolve(resolvable text, resolved bigint[], error_message text)
  returns bigint as $$
begin
  if array_length(resolved, 1) > 1 or resolvable is not null and resolved[1] is null then
    raise exception sqlstate '235BX' using message = error_message;
  else
    return resolved[1];
  end if;
end;
$$ language plpgsql immutable;
```
(and the same for `resolved text[]`).
[ScalaDtoGenerator](#dtos) also emits `resolve_<column>` methods on the DTO and companion.

### Validations

View extra **validations** is a list of tresql expressions. Each item is a comma-separated
list: optional cursor definitions, a boolean requirement, an error message expression,
and optional error message parameter expressions. A validation fails if the boolean is not
true. If the message is null or empty, it defaults to `Requirement failed: "<require>"`.
Child views are validated too; `:__parent` is the parent row.

Message parameters are for localization. The error message is a static template — usable as
an i18n resource key — with placeholders `%1$s`, `%2$s`, … in `java.lang.String.format`
syntax. Querease does not translate; it returns `ValidationMessage(msg, params)`.
[wabase](https://github.com/mrumkovskis/wabase/blob/master/docs/view-actions.md#message-parameters)
looks the template up in an i18n bundle and substitutes the parameters. Put values into the
message through placeholders, not by concatenation — a message containing values cannot be
found in the bundle.

```yaml
name:     validations_test
table:    validations_test_table
fields:
- id
- integer_column
- name_col
validations:
- :integer_column > 5, 'integer_column should be greater than %1$s but is %2$s', 5, :integer_column::int
- :name_col != 'forbidden', 'Code is forbidden'
```

`validate` / `save` throw
[ValidationException](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ValidationException.html)
when any message is non-empty. `getMessage` is the messages joined with newlines; `details`
keeps location, message and parameters.

`build cursors` or `build cursors for view <name>` as the first item materializes the
saved data (including children) as SQL cursors via the `build_cursors` macro, so later
expressions can query children. Cursor definitions can be used in the require, message and
parameter expressions. `^view.field` in a validation is replaced with that field's
query expression.

### Initial values

Field extra **initial** is a tresql expression used by **create** to populate a new instance.

```yaml
name:   person_2
table:  person
fields:
- id
- full_name:
  - initial: "'Name Surname'"
```

```scala
qe.create[person_2]()           // full_name = "Name Surname"
qe.create[person_2](params)     // params bound in initial expressions
```

## DTOs

[ScalaDtoGenerator](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ScalaDtoGenerator.html)
extends mojoz [ScalaGenerator](https://static.javadoc.io/org.mojoz/mojoz_3/7.2.0/org/mojoz/metadata/out/ScalaGenerator.html):
classes extend [Dto](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/Dto.html)
or [DtoWithId](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/DtoWithId.html)
(when the view has a non-optional `id` of a supported integer type), and resolver methods are
added on the instance and companion.

```yaml
name:    person
table:   person p
fields:
- id
- name
- surname
- mother.name
- father.name
- children * [p.id in (c.mother_id, c.father_id)] person_name
```
```scala
class person extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var surname: String = null
  var mother_name: String = null
  var father_name: String = null
  var children: List[person_name] = Nil
}
```

[Dto](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/Dto.html).fill
populates from a tresql `RowLike` or a `Map`. `toMap` uses view field order.
[ScalaDtoQuereaseIo](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ScalaDtoQuereaseIo.html)
implements [QuereaseIo](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseIo.html)
via `fill` / `toMap`. [ValueConverter](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/ValueConverter.html)
converts between JVM/SQL types (dates, numbers, bytes as Base64, UUID, and nested maps/seqs).
`data.timezone` in Typesafe Config selects the zone for datetime conversion.

DTO generation is typically wired through [sbt-mojoz](https://github.com/guntiso/sbt-mojoz):
```scala
mojozScalaGenerator := Def.uncached(new org.mojoz.querease.ScalaDtoGenerator(mojozQuerease.value) {
  override def scalaClassName(name: String): String =
    name.split("[_\\-\\.]+").toList.map(_.toLowerCase.capitalize).mkString
})
```
Override `viewNameFromMf` if class names do not match view names.

## JSON columns

A view with **column** describes the structure of a JSON (or yaml) table column
(see [mojoz view metadata](https://github.com/guntiso/mojoz#view-metadata)).
On query, that column is selected as a whole and unpacked into fields.
On save, non-table fields are packed back into the JSON column.

```yaml
name:  column_test
table: json_col_test
column: payload
fields:
- id
- whatever
- nested_thing
```

A child view can also be stored in a JSON column (`children * = value` with an inline view).
`-> = _` on such a child resolves the JSON value from the child data.

## Multi-database

View extra **db** selects a tresql connection / database. Child views inherit **db** from the
parent unless **db** is set (including empty to use the default database).

```yaml
name:   person_2
db:     querease2
table:  person
fields:
- id
- full_name
```

Database aliases are read from `tresql-resources.conf` or `reference.conf`
(`tresql.<alias>` objects, `tresql.default`, `tresql.db`, `tresql.macros-class`).
See [QuereaseMetadata](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/QuereaseMetadata$.html).aliasToDb
and [TresqlMetadata](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease/TresqlMetadata.html).

## [API docs](https://static.javadoc.io/org.mojoz/querease_3/10.2.1/org/mojoz/querease.html)
