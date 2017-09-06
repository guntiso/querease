package querease

import org.tresql.QueryParser
import org.tresql.QueryParser.Ident
import org.tresql.QueryParser.Variable

trait QuereaseResolvers { this: Querease =>

        def allResolvers(view: ViewDef, f: FieldDef) =
          allResolversRaw(view, f)
            .map(transformResolver(view, f, _))

        def allResolversRaw(view: ViewDef, f: FieldDef) = {

          val name = f.name
          val alias = Option(f.alias).getOrElse(f.name)
          val saveToMulti = view.saveTo != null && view.saveTo.size > 0
          val saveTo = if (!saveToMulti) Seq(view.table) else view.saveTo

          // TODO view validation - validate f.saveTo and resolvers!
          // TODO support refs to ^view.field for expressions and/or resolvers
          // TODO support save-to table or alias qualifier
          // TODO support some qualifiers in expression?
          def explicitResolvers(f: FieldDef) =
            Option(f.resolver)
              .map(QueryParser.parseExp)
              .map(QueryParser.transformer {
                case v@Variable(a, _, _, _) if a == alias =>
                  v.copy(variable = alias + "->")
              })
              .map(_.tresql)
              .map(r => Seq(r))
          def impliedResolvers(f: FieldDef, doRebaseTable: Boolean) = {
            val fSaveTo = Option(f.saveTo) getOrElse name
            saveTo
              .map(tableMetadata.tableDef)
              .filter(_.cols.exists(_.name == fSaveTo))
              .flatMap(_.refs.filter(_.cols == Seq(fSaveTo)))
              .map { ref =>
                val refTable = ref.refTable
                val refCol = ref.refCols(0)
                val expressionOpt =
                  if (doRebaseTable)
                    Option(f.expression)
                    .map(QueryParser.parseExp)
                    .map(QueryParser.transformer {
                      case i: Ident =>
                        if (i.ident.size > 1) i.copy(ident = i.ident.tail) else i
                    })
                    .map(_.tresql)
                  else
                    Option(f.expression)
                val expression = expressionOpt.getOrElse(name)
                s"$refTable[$expression = _]{$refCol}"
              }
          }
          def impliedSelfResolvers(f: FieldDef) = {
            val fSaveTo = Option(f.saveTo) getOrElse name
            saveTo
              .map(tableMetadata.tableDef)
              .filter(_.cols.exists(_.name == fSaveTo))
              .filter(t =>
                t.pk.exists(_.cols == Seq(fSaveTo)) ||
                t.uk.exists(_.cols == Seq(fSaveTo)))
              .map { t =>
                val expression = Option(f.expression).getOrElse(name)
                s"${t.name}[$expression = _]{$fSaveTo}"
              }
          }
          def impliedRefResolvers(f: FieldDef, refViewDef: ViewDef, refFieldDef: FieldDef) = {
            val fSaveTo = Option(f.saveTo) getOrElse name
            saveTo
              .map(tableMetadata.tableDef)
              .filter(_.cols.exists(_.name == fSaveTo))
              .flatMap(_.refs.filter(_.cols == Seq(fSaveTo)))
              .map { ref =>
                val refTable = ref.refTable
                val refCol = ref.refCols(0)
                val expression = Option(refFieldDef.expression).getOrElse(refFieldDef.name)
                s"$refTable[$expression = _]{$refCol}"
                // TODO check refTable in view tables?
                val refColField =
                  (new mojoz.metadata.FieldDef(refCol))
                    .copy(table = refViewDef.table, tableAlias = refViewDef.tableAlias)
                queryString(refViewDef, refColField, refFieldDef, s"$expression = _")
              }
          }
          def referencedResolvers =
            Option(f.expression)
              .filter(FieldRefRegexp.pattern.matcher(_).matches)
              .map {
                case FieldRefRegexp(refViewName, refFieldName, _, _) =>
                  val refViewDef = viewDefOption(refViewName)
                    .getOrElse{
                      throw new RuntimeException(
                        s"View $refViewName referenced from ${view.name}.$alias is not found")
                    }
                  val refFieldDef = refViewDef.fields
                    .filter(f => Option(f.alias).getOrElse(f.name) == refFieldName)
                    .headOption
                    .getOrElse {
                      throw new RuntimeException(
                        s"Field $refViewName.$refFieldName referenced from ${view.name}.$alias is not found")
                    }
                  explicitResolvers(refFieldDef)
                    .orElse(Option(impliedResolvers(refFieldDef, false)).filter(_.size > 0))
                    .getOrElse(impliedRefResolvers(f, refViewDef, refFieldDef))
              }

          if (f.saveTo != null || f.resolver != null)
            explicitResolvers(f)
              .orElse(referencedResolvers)
              .orElse(Option(impliedResolvers(f, true)).filter(_.size > 0))
              .getOrElse(impliedSelfResolvers(f))
          else
            Nil
        }
}
