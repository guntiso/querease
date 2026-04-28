# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Querease is a Scala library that provides metadata-driven database query and persistence. It bridges YAML-based metadata definitions (via mojoz) with Tresql (a SQL DSL) to generate type-safe queries, DTO classes, and persistence operations.

## Build & Test Commands

**Build** (cross-compiled for Scala 2.12, 2.13, and 3):
```bash
sbt compile          # current Scala version
sbt +compile         # all cross versions
```

**Run tests**:
```bash
sbt test
sbt "testOnly test.QuereaseTests"
sbt "testOnly test.QuereaseDbTests"
sbt "testOnly test.FilterResolverTests"
sbt "testOnly test.ValueConverterTests"
sbt "testOnly test.CursorsTests"
```

**Binary compatibility check** (run before releasing):
```bash
sbt versionPolicyCheck
```

**CI-style full build** (requires PostgreSQL `querease` user/database with UTF-8 collation):
```bash
cp test/conf/application.conf.ci test/conf/application.conf
sbt -Dhsqldb.method_class_names="test.HsqldbCustomFunctions.*" clean update +compile +test +versionPolicyCheck
```

PostgreSQL tests are skipped by default unless `test/conf/application.conf` enables them. HSQLDB tests always run.

## Architecture

The library is built from composable traits mixed into a single `Querease` trait:

```
QuereaseMetadata + QuereaseExpressions + QuereaseResolvers
+ QuereaseFilters + FilterTransformer + ValueTransformer
+ QueryStringBuilder
      ↓
  Querease (main API trait)
      ↓
  ScalaDtoQuerease (adds DTO reflection-based I/O)
```

**Data flow**:
1. YAML files (`test/tables/`, `test/views/`, `test/types/`) → mojoz loads `TableDef`/`ViewDef` metadata
2. `Querease.queryStringAndParams(viewDef, filters)` → Tresql query string
3. Tresql executes query → `Result` (row-like)
4. `ScalaDtoQuerease.rowLikeToDto()` → typed DTO instance

### Core source files (`src/`)

- **Querease.scala** — Main API: `query()`, `search()`, `read()`, `save()`, `delete()`, `ValueConverter` (50+ type conversions), validation, filtering
- **QuereaseMetadata.scala** — Metadata caching, persistence metadata generation for Tresql ORT (Object-Relational Transformation), key field detection, `saveToMap` construction
- **QuereaseExpressions.scala** — Tresql expression parsing/caching, variable extraction, context-aware transformation (Field/Filter/Resolver/Validation contexts)
- **ScalaDtoQuerease.scala** — Reflection-based DTO deserialization; caches field setters; handles `Option`, `Seq`, and nested DTOs
- **ScalaDtoGenerator.scala** — Generates Scala DTO case classes and resolver companion methods from `ViewDef`
- **QuereaseMacros.scala** — Tresql macro implementations: `build_cursors` for hierarchical data, `__row_nr`/`__row_nr_ref` row numbering
- **TresqlMetadata.scala** — Adapts mojoz `TableDef` to Tresql's `Metadata` interface; multi-database alias support
- **TresqlJoinsParser.scala** — Compiles join expressions via Tresql compiler; caches per database; CTE support
- **QuereaseResolvers.scala** — Resolves fields from foreign keys, unique keys, or explicit resolver expressions
- **QuereaseFilters.scala** — Filter types (`BooleanFilter`, `IdentFilter`, `ComparisonFilter`, `IntervalFilter`, `RefFilter`) and transformation

### Key abstractions

| Type | Description |
|---|---|
| `ViewDef` | Queryable view with fields, joins, table mapping |
| `FieldDef` | Field with type, resolver, `saveTo`, expression |
| `OrtMetadata.View` | Persistence plan for insert/update/delete via Tresql ORT |
| `FieldFilter` | Controls which fields are included and recursed into |
| `QuereaseIteratorResult` | Lazy, auto-closing iterator over query results |
| `ValueConverter` | Bidirectional conversion for 50+ JVM/SQL types |

### Multi-database support

Extra databases are configured via `tresql.{dbName}` Typesafe Config entries. `TresqlMetadata` maps alias names to database names via `aliasToDb: Map[String, String]`.

### Scala 2.12 compatibility

The `compat/scala-2.12/` directory contains shims (collection converters) needed for the 2.12 cross-build.
