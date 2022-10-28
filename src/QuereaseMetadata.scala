package org.mojoz.querease

import org.mojoz.metadata._
import org.mojoz.metadata.TableDef.{Ref, TableDefBase}
import org.mojoz.metadata.ColumnDef.ColumnDefBase
import org.mojoz.metadata.in.{YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import org.mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}

import org.tresql.ast.{Ident, Variable}
import org.tresql.MacroResourcesImpl
import org.tresql.QueryParser
import org.tresql.OrtMetadata
import org.tresql.OrtMetadata._

import scala.collection.immutable.{Map, ListSet, Seq, TreeMap}
import scala.util.matching.Regex

case class ViewNotFoundException(message: String) extends Exception(message)
case class FieldOrderingNotFoundException(message: String) extends Exception(message)

trait QuereaseMetadata { this: QuereaseExpressions with QuereaseResolvers with QueryStringBuilder =>

  type FieldDef = org.mojoz.metadata.FieldDef[Type]
  type ViewDef = org.mojoz.metadata.ViewDef[FieldDef]

  class FieldOrdering(val nameToIndex: Map[String, Int]) extends Ordering[String] {
    override def compare(x: String, y: String) =
      nameToIndex.getOrElse(x, 999) - nameToIndex.getOrElse(y, 999) match {
        case 0 => x compareTo y
        case d => d
      }
    override def toString: String =
      s"FieldOrdering(${nameToIndex.toList.sortBy(_._2).map(_._1).mkString(", ")})"
  }
  object FieldOrdering {
    def apply(view: ViewDef) =
      new FieldOrdering(view.fields.map(_.fieldName).zipWithIndex.toMap)
  }

  protected lazy val yamlMetadata = YamlMd.fromResources()
  lazy val metadataConventions: MdConventions = new SimplePatternMdConventions
  lazy val typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs
  lazy val tableMetadata: TableMetadata[TableDefBase[ColumnDefBase[Type]]] =
    new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions, typeDefs).tableDefs)
  lazy val macrosClass: Class[_] = null
  lazy val tresqlMetadata = TresqlMetadata(tableMetadata.tableDefs, typeDefs, macrosClass)
  protected lazy val tresqlJoinsParser = new TresqlJoinsParser(tresqlMetadata)

  lazy val viewDefLoader: YamlViewDefLoader =
    YamlViewDefLoader(tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions, Nil, typeDefs)
  import QuereaseMetadata.toQuereaseViewDefs
  lazy val nameToViewDef: Map[String, ViewDef] = toQuereaseViewDefs {
    viewDefLoader
      .nameToViewDef.asInstanceOf[Map[String, ViewDef]]
  }
  // TODO merge all extra metadata, attach to view: ordering, persistence metadata, key fields, ...
  protected lazy val viewNameToFieldOrdering = nameToViewDef.map(kv => (kv._1, FieldOrdering(kv._2)))
  protected lazy val persistenceMetadataMaxDepth: Int = -1
  protected lazy val nameToPersistenceMetadata: Map[String, OrtMetadata.View] =
    nameToViewDef.flatMap { case (name, viewDef) =>
      (try toPersistenceMetadata(viewDef, nameToViewDef) catch {
        case util.control.NonFatal(ex) => None
      }).toSeq.map(name -> _)
    }
  lazy val viewNameToMapZero: Map[String, Map[String, Any]] =
    nameToViewDef.map { case (name, viewDef) =>
      (name, TreeMap[String, Any]()(viewNameToFieldOrdering(name)) ++
        viewDef.fields.map(f => (f.fieldName, if (f.isCollection) Nil else null)))
    }
  lazy val viewNameToKeyFields: Map[String, Seq[FieldDef]] =
    nameToViewDef.map { case (name, viewDef) => (name, keyFields(viewDef)) }
  lazy val viewNameToKeyFieldNames: Map[String, Seq[String]] =
    viewNameToKeyFields.map { case (name, fields) => (name, fields.map(_.fieldName)) }
  lazy val viewNameToKeyColNames: Map[String, Seq[String]] =
    viewNameToKeyFields.map { case (name, fields) => (name, fields.map(_.name)) }
  lazy val viewNameToKeyColNameForGetById: Map[String, String] =
    nameToViewDef.map { case (name, viewDef) => (name, keyColNameForGetById(viewDef)) }
  lazy val viewNameToKeyColNameForGetByCode: Map[String, String] =
    nameToViewDef.map { case (name, viewDef) => (name, keyColNameForGetByCode(viewDef)) }
  lazy val viewNameToIdFieldName: Map[String, String] =
    nameToViewDef.map { case (name, viewDef) => (name, idFieldName(viewDef)) }.filter(_._2 != null).toMap

  def fieldOrderingOption(viewName: String): Option[Ordering[String]] = viewNameToFieldOrdering.get(viewName)
  def fieldOrdering(viewName: String): Ordering[String] = fieldOrderingOption(viewName)
    .getOrElse(throw FieldOrderingNotFoundException(s"Field ordering for view $viewName not found"))
  def fieldOrderingOption[T <: AnyRef: Manifest]: Option[Ordering[String]] = fieldOrderingOption(viewName[T])
  def fieldOrdering[T <: AnyRef](implicit mf: Manifest[T]): Ordering[String] = fieldOrderingOption[T]
    .getOrElse(throw FieldOrderingNotFoundException(s"Field ordering for view $mf not found"))
  def viewDefOption(viewName: String): Option[ViewDef] = nameToViewDef.get(viewName)
  def viewDef(viewName: String) = viewDefOption(viewName)
    .getOrElse(throw ViewNotFoundException(s"View definition for $viewName not found"))
  def viewDefOption[T <: AnyRef: Manifest]: Option[ViewDef] = viewDefOption(viewName[T])
  def viewDef[T <: AnyRef](implicit mf: Manifest[T]): ViewDef =
    viewDefOption[T].getOrElse(throw ViewNotFoundException(s"View definition for type $mf not found"))

  def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
    mf.runtimeClass.getSimpleName

  protected def keyFields(view: ViewDef): Seq[FieldDef] = {
    import QuereaseMetadata.AugmentedQuereaseViewDef
    Option(view.keyFieldNames)
      .filter(_.nonEmpty)
      .map(_.map { fieldName =>
        view.fieldOpt(fieldName).getOrElse(sys.error(s"Custom key field $fieldName not found, view ${view.name}"))
      }) getOrElse
    Option(view.table)
      .map(tableMetadata.tableDef(_, view.db))
      .flatMap { t =>
        ((if (t.pk != null) t.pk.toSeq else Nil) ++
         (if (t.uk != null) t.uk       else Nil)
        ).filter(_ != null)
         .find { k => k.cols forall(col => view.fields.exists(f => f.table == view.table && f.name == col))     }
      } .map   { k => k.cols.map   (col => view.fields.find  (f => f.table == view.table && f.name == col).get) }
        .getOrElse(Nil)
  }

  protected def keyColNameForGetById(view: ViewDef): String =
    Option(view.table)
      .map(tableMetadata.tableDef(_, view.db))
      .flatMap { t =>
        ((if (t.pk != null) t.pk.toSeq else Nil) ++
         (if (t.uk != null) t.uk       else Nil)
        ).filter(_ != null)
         .find { k => k.cols.size == 1 && t.cols.exists(col => col.name == k.cols.head && col.type_.name == "long") }
      } .map(_.cols.head)
        .orNull

  protected def keyColNameForGetByCode(view: ViewDef): String =
    Option(view.table)
      .map(tableMetadata.tableDef(_, view.db))
      .flatMap { t =>
        ((if (t.pk != null) t.pk.toSeq else Nil) ++
         (if (t.uk != null) t.uk       else Nil)
        ).filter(_ != null)
         .find { k => k.cols.size == 1 && t.cols.exists(col => col.name == k.cols.head && col.type_.name == "string") }
      } .map(_.cols.head)
        .orNull

  val supportedIdTypeNames: Set[String] = ListSet("long", "int", "short")
  protected def idFieldName(view: ViewDef): String =
    tableMetadata.tableDefOption(view.table, view.db).flatMap { t =>
      t.pk
        .map(_.cols)
        .filter(_.size == 1)
        .map(_.head)
        .flatMap { pk =>
          viewNameToKeyFields(view.name).find { f =>
            f.table == t.name && f.name == pk && supportedIdTypeNames.contains(f.type_.name)
          }.map(_.fieldName)
        }
    }.orNull

  protected def identifier(s: String) = s match {
    case QuereaseExpressions.IdentifierExtractor(ident, _) => ident
    case _ => ""
  }
  protected def isSaveableSimpleField(
    field: FieldDef,
    view: ViewDef,
    saveToMulti: Boolean,
    saveToTableNames: Seq[String],
  ) = {
    (
      field.saveTo != null        &&
      field.saveTo.indexOf(':') < 0
      ||
      field.resolver != null
      ||
      !field.isExpression         &&
      !field.type_.isComplexType  &&
      field.table != null         &&
      (
        !saveToMulti                &&
        field.table == view.table   &&
        (
          field.tableAlias == null    ||
          field.tableAlias == view.tableAlias
        )
        ||
        saveToMulti &&
        saveToTableNames.contains(field.table) // TODO alias?
      )
    ) && (
      isFieldForInsert(field) ||
      isFieldForUpdate(field)
    )
  }
  protected def fieldOptionsSelf(field: FieldDef) =
    Option(field.options)
      .map(_.replace("[", "").replace("]", ""))
      .map(_.split("/", 2))
      .map { arr =>
        if  (arr.length == 0 ||
             arr.length == 1 && field.type_.isComplexType && !arr(0).contains('!'))
             null
        else arr(0)
      }
      .filter(_ != "")
      .orNull
  protected def fieldOptionsRef(field: FieldDef) =
    Option(field.options)
      .map(_.replace("[", "").replace("]", ""))
      .map(_.split("/", 2))
      .map { arr => if (arr.length == 0) null else arr(arr.length - 1) }
      .filter(_ != "")
      .orNull
  protected def isSaveableRefToReadonlyChildField(field: FieldDef): Boolean = {
    val opt_self  = fieldOptionsSelf(field)
    val opt_ref   = fieldOptionsRef(field)
    field.type_.isComplexType && !field.isCollection &&
      (opt_self == null || !opt_self.contains('!'))  &&
       opt_ref  != null &&  opt_ref.contains('!')
  }
  protected def isSaveableChildField(
    field: FieldDef,
    view: ViewDef,
    saveToMulti: Boolean,
    saveToTableNames: Seq[String],
    childView: ViewDef,
  ): Boolean = {
    val opt = fieldOptionsRef(field)
    opt == null || !opt.contains('!')
  }
  protected def isFieldForInsert(field: FieldDef): Boolean = {
    val opt = fieldOptionsSelf(field)
    opt == null || opt.contains('+') && !opt.contains('!')
  }
  protected def isFieldForUpdate(field: FieldDef): Boolean = {
    val opt = fieldOptionsSelf(field)
    opt == null || opt.contains('=') && !opt.contains('!')
  }
  protected def persistenceFilters(
    view: ViewDef,
  ): Filters = {
    Filters(
      insert = None,
      update = None,
      delete = None,
    )
  }
  protected def hasExplicitKey(view: ViewDef) = {
    import QuereaseMetadata.AugmentedQuereaseViewDef
    Option(view.keyFieldNames)
      .filter(_.nonEmpty)
      .isDefined
  }
  // FIXME remove isKeyValueSupported_ - waiting for https://github.com/mrumkovskis/tresql/issues/42
  protected def isKeyValueSupported_(view: ViewDef, keyFields: Seq[FieldDef], saveToTables: Seq[TableDefBase[_]]) = {
    hasExplicitKey(view) ||
    !keyFields.exists { f =>
      saveToTables.exists(_.pk.map(_.cols.contains(f.name)) getOrElse false)
    }
  }
  def oldKeyParamName = "old key"
  private lazy val oldKeyRef = Variable(oldKeyParamName, Nil, opt = false).tresql
  protected def toPersistenceMetadata(
    view: ViewDef,
    nameToViewDef:  Map[String, ViewDef],
    parentNames:    List[String]  = Nil,
    refsToParent:   Set[String]   = Set.empty,
    maxDepth:       Int           = persistenceMetadataMaxDepth,
  ): Option[OrtMetadata.View] = try {
    if (maxDepth == 0)
      return None
    if (maxDepth  < 0 && parentNames.contains(view.name))
      // provide Ref(view_name) instead - when supported by tresql
      sys.error("Persistence metadata of unlimited depth for recursive structure not supported: " +
        (view.name :: parentNames).reverse.mkString(" -> "))
    val keyFields = if (parentNames == Nil) viewNameToKeyFields(view.name) else Nil
    val saveToMulti = view.saveTo != null && view.saveTo.nonEmpty
    def saveToNames(view: ViewDef) =
      Option(view.saveTo).getOrElse(Seq(view.table)).filter(_ != null)
    val saveToTableNames = saveToNames(view).map(identifier)
    val saveToTableNamesWithRefsAndKeys = saveToNames(view)
    def isSaveableField_(field: FieldDef): Boolean =
      isSaveableSimpleField(field, view, saveToMulti, saveToTableNames)
    def isSaveableChildField_(field: FieldDef, childView: ViewDef) =
      isSaveableChildField(field, view, saveToMulti, saveToTableNames, childView)
    def childSaveTo(field: FieldDef, childView: ViewDef) =
      Option(field).map(_.saveTo).filter(_ != null) getOrElse saveToNames(childView)
    def isChildTableField(field: FieldDef) =
      !field.isExpression &&
        field.type_.isComplexType &&
        nameToViewDef.get(field.type_.name).map(saveToNames).orNull != null
    // TODO duplicate code - saveTo(), multiSaveProp() copied from tresql
    import org.tresql.Metadata
    def saveTo_(tables: Seq[String], md: Metadata) = {
      def multiSaveProp(names: Seq[String]) = {
        /* Returns zero or one imported key from table for each relation. In the case of multiple
         * imported keys pointing to the same relation the one specified after : symbol is chosen
         * or exception is thrown.
         * This is used to find relation columns for insert/update multiple methods.
         * Additionaly, primary key of table is returned if it consists of only one column */
        def importedKeysAndPks(tableName: String, relations: List[String]) = {
          val x = tableName split ":"
          val table = md.table(x.head)
          relations.foldLeft(x.tail.toSet) { (keys, rel) =>
            val relation = rel.split(":").head
            val refs = table.refs(relation).filter(_.cols.size == 1)
            (if (refs.size == 1) keys + refs.head.cols.head
            else if (refs.isEmpty || refs.exists(r => keys.contains(r.cols.head))) keys
            else sys.error(s"Ambiguous refs in view ${view.name}: $refs from table ${table.name} to table $relation")) ++
              (table.key.cols match {case List(k) => Set(k) case _ => Set()})
          }
        }
        names.tail.foldLeft(List(names.head)) { (ts, t) => (t.split(":")
          .head + importedKeysAndPks(t, ts).mkString(":", ":", "")) :: ts
        }.reverse
      }
      tables.map { t =>
        val ki = t.indexOf("[")
        if (ki != -1) {
          (t.substring(0, ki), t.substring(ki + 1, t.length - 1).split("\\s*,\\s*").toList)
        } else (t, Nil)
      }.unzip match {
        case (names, keys) => multiSaveProp(names.toIndexedSeq) zip keys map { case (table, key) =>
          val t = table.split(":")
          SaveTo(t.head, t.tail.toSet, key)
        }
      }
    }
    val saveTo =
      // TODO wt?
      if (saveToTableNamesWithRefsAndKeys.lengthCompare(1) > 0 ||
          saveToTableNamesWithRefsAndKeys.lengthCompare(0) > 0 && (
            saveToTableNamesWithRefsAndKeys.head.indexOf(':') >= 0 ||
            saveToTableNamesWithRefsAndKeys.head.indexOf('[') >= 0
          )
         )
        saveTo_(saveToTableNamesWithRefsAndKeys, tresqlMetadata)
      else {
        val keyCols = viewNameToKeyColNames(view.name)
        saveToTableNames.map(t => SaveTo(
          table = t,
          refs  = refsToParent,
          key   = tableMetadata.tableDef(t, view.db).pk.map { pk =>
            if (!hasExplicitKey(view) && pk.cols == keyCols) Nil else keyCols // TODO annoying, improve tresql?
          }.getOrElse(Nil),
        ))
      }
    val filtersOpt = Option(persistenceFilters(view))
    lazy val tables = saveToTableNames.map(tableMetadata.tableDef(_, view.db))
    val isKeyValueSupported = // FIXME remove - waiting for https://github.com/mrumkovskis/tresql/issues/42
      isKeyValueSupported_(view, keyFields, tables)
    val properties = view.fields
      .map { f => try {
        val fieldName = f.fieldName
        def bestLookupRefs = {
          // TODO child multi-tables?
          val childTableName = nameToViewDef.get(f.type_.name).flatMap(saveToNames(_).headOption).orNull
          // TODO searh also reverse refs to avoid runtime exceptions
          // TODO raise more ambiguities, refs search too liberal and thus unstable when db structure changes
          if  (f.isCollection) Nil
          else tables
            .sorted( // sort is stable
              Ordering.by((table: org.mojoz.metadata.TableDef.TableDefBase[_]) =>
                if (table.name == f.table) 0 else 1))
            .find { table => table.refs.exists(_.refTable == childTableName) }
            .map { table => table -> table.refs.count(_.refTable == childTableName) }
            .map {
              case (table, 1) =>
                table.refs.find(_.refTable == childTableName).toSeq
              case (table, n) =>
                table.refs.find { t => t.refTable == childTableName &&
                  t.defaultRefTableAlias == fieldName
                }.toSeq
            }
            .getOrElse(Nil)
        }
        def persistencePropertyValue(valueTresql: String) = {
          val opt = fieldOptionsSelf(f)
          val tresqlValue = TresqlValue(
            tresql    = valueTresql,
            forInsert = opt == null || opt.contains('+') && !opt.contains('!'),
            forUpdate = opt == null || opt.contains('=') && !opt.contains('!'),
            optional  = false, // TODO [optional]?
          )
          if (keyFields.contains(f) && isKeyValueSupported)
            KeyValue(
              whereTresql = s"if_defined_or_else($oldKeyRef.$fieldName?, $oldKeyRef.$fieldName?, :$fieldName)",
              valueTresql = tresqlValue,
            )
          else tresqlValue
        }
        val childView =
          if (f.type_.isComplexType) {
            val childViewName = f.type_.name
            val childView = nameToViewDef.getOrElse(childViewName,
              sys.error(s"View $childViewName referenced from ${view.name} not found")
            )
            childView
          } else null
        if (isSaveableField_(f)) {
          val saveTo    = Option(f.saveTo).getOrElse(f.name)
          val valueTresql =
            if (f.saveTo == null && f.resolver == null)
              ":" + fieldName
            else {
              val resolvers = allResolvers(view, f)
              if (resolvers.nonEmpty) {
                val resolver = resolvers.head
                parser.transformer {
                  case Ident(List("_")) => Variable(fieldName, Nil, opt = false)
                } (parser.parseExp(if (resolver startsWith "(" ) resolver else s"(${resolver})")).tresql
              } else {
                f.saveTo
              }
            }
          Property(
            col   = saveTo,
            value = persistencePropertyValue(valueTresql),
          )
        } else if (isSaveableRefToReadonlyChildField(f)) {
          bestLookupRefs match {
            case Nil =>
              sys.error(s"Reference not found for field ${view.name}.${f.name}")
            case Seq(ref) =>
              // TODO support multi-col refs here?
              if (ref.cols.size != 1)
                sys.error(s"Multi-column reference not supported to persist field ${view.name}.${f.name}")
              val refColName = ref.cols.head
              val refFieldDef = new FieldDef(ref.refCols.head).copy(table = ref.refTable)
              val childKey = viewNameToKeyFields(childView.name)
              def bindVarName(cf: FieldDef) =
                s"${f.fieldName}.${cf.fieldName}"
              val childKeyFilter = childKey.map(cf => s"${cf.name} = :${bindVarName(cf)}").mkString(" & ")
              // TODO extra filter (auth filter) for child view "get"?
              val filter = childKeyFilter
              val refQuery = queryString(childView, Seq(refFieldDef), Nil, filter)
              val contextName = "resolver"
              val resolvables = childKey.map(cf => (s":${bindVarName(cf)}", Option(cf.type_))).toList // TODO allow seq!
              val resolvablesExpr = resolvablesExpression(view.name, fieldName, contextName, resolvables)
              val errorMessage = resolverErrorMessageExpression(view.name, fieldName, contextName, resolvables)
              val resolverExpr = resolverExpression(resolvablesExpr, refQuery, errorMessage)
              Property(
                col   = refColName,
                value = persistencePropertyValue(s"($resolverExpr)"),
              )
            case refs =>
              sys.error(s"Ambiguous references for field ${view.name}.${f.name}")
          }
        } else if (isChildTableField(f) && isSaveableChildField_(f, childView)) {
          val opt = fieldOptionsRef(f)
          val childSaveOptions = SaveOptions(
            doInsert = (opt == null) || (opt contains '+') && !opt.contains('!'),
            doUpdate = (opt != null) && (opt contains '=') && !opt.contains('!'), // TODO invert default for doUpdate when child has key?
            doDelete = (opt == null) || (opt contains '-') && !opt.contains('!'),
          )
          val childSaveTo =
            if (f.saveTo != null && f.saveTo.contains(':'))
              saveTo_(Seq(f.saveTo), tresqlMetadata)
            else null
          toPersistenceMetadata(
            view          = childView,
            nameToViewDef = nameToViewDef,
            parentNames   = view.name :: parentNames,
            refsToParent  = Set.empty,
            maxDepth      = maxDepth - 1,
          )
          .map(md => if (childSaveTo != null) md.copy(saveTo = childSaveTo) else md)
          .filter(_.saveTo.nonEmpty)
          .map(_.copy(
            forInsert = isFieldForInsert(f),
            forUpdate = isFieldForUpdate(f),
          ))
          .map { childPersistenceMetadata =>
            bestLookupRefs match {
              case Nil =>
                Property(
                  col   = f.name,
                  value = ViewValue(childPersistenceMetadata, childSaveOptions)
                )
              case Seq(ref) =>
                // TODO prefer single-col ref, throw for multi-col
                val refColName = ref.cols.head
                Property(
                  col   = refColName,
                  value = LookupViewValue(fieldName, childPersistenceMetadata)
                )
              case refs =>
                sys.error(s"Ambiguous references for field ${view.name}.${f.name}")
            }
          }.orNull
        } else {
          null
        }
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Failed to build persistenceMetadata for field ${f.fieldName}", ex)
      }}
      .filter(_ != null)
    val persistenceMetadata =
      OrtMetadata.View(
        saveTo      = saveTo,
        filters     = filtersOpt,
        alias       = view.tableAlias,
        forInsert   = true,
        forUpdate   = true,
        optional    = false, // TODO [optional]?
        properties  = properties,
        db          = view.db,
      )
    return Option(persistenceMetadata)
  } catch {
    case util.control.NonFatal(ex) =>
      throw new RuntimeException(s"Failed to build persistenceMetadata for view ${view.name}", ex)
  }

  def persistenceMetadata(view: ViewDef, data: Map[String, Any]): OrtMetadata.View = {
    def maxDataDepth(data: Map[String, Any]): Int = data.values.foldLeft(1) {
      case (mx1, child: Map[String@unchecked, _]) =>
        math.max(mx1, maxDataDepth(child) + 1)
      case (mx1, children: Iterable[_]) =>
        math.max(
          mx1,
          children.foldLeft(mx1) {
            case (mx2, child: Map[String@unchecked, _]) =>
              math.max(mx2, maxDataDepth(child) + 1)
            case (mx2, _) => mx2
          }
        )
      case (mx1, _) => mx1
    }
    def dynamicPersistenceMetadata =
      toPersistenceMetadata(
        view,
        nameToViewDef,
        maxDepth = maxDataDepth(data) + 1 // +1 for potential cleanup of lookup and/or children
      ).get
    nameToPersistenceMetadata.getOrElse(view.name, dynamicPersistenceMetadata)
  }
}

object QuereaseMetadata {
  trait QuereaseViewDefExtras {
    // val keyFieldNames:         Seq[String] // TODO Minor binary incompatible. Why this trait, remove it?
    val validations: Seq[String]
  }

  private [querease] case class QuereaseViewDef(
    keyFieldNames: Seq[String] = Nil,
    validations: Seq[String] = Nil
  ) extends QuereaseViewDefExtras

  trait QuereaseFieldDefExtras {
    val initial: String
  }

  private [querease] case class QuereaseFieldDef(
    initial: String = null
  ) extends QuereaseFieldDefExtras

  val BindVarCursorsCmd = "build cursors"
  val BindVarCursorsCmdRegex =
    new Regex(BindVarCursorsCmd + """((\s+:\w+)*)""")
  val BindVarCursorsForViewCmd = "build cursors for view"
  val BindVarCursorsForViewCmdRegex =
    new Regex(BindVarCursorsForViewCmd + """\s+(:?\w+)((\s+:\w+)*)""")

  val QuereaseViewExtrasKey = "querease-view-extras"
  val QuereaseFieldExtrasKey = "querease-field-extras"
  trait ExtrasMap {
    protected def updateExtrasMap(extras: Map[String, Any]): Any
    protected def extrasMap: Map[String, Any]

    private def extrasMapOrEmpty: Map[String, Any] = Option(extrasMap).getOrElse(Map())
    protected def updateExtras[T, E](key: String, updater: E => E, default: E): T =
      updateExtrasMap(extrasMapOrEmpty + (key -> updater(extras(key, default)))).asInstanceOf[T]
    protected def extras[E](key: String, default: E): E = extrasMapOrEmpty.getOrElse(key, default).asInstanceOf[E]
  }
  implicit class AugmentedQuereaseViewDef(viewDef: QuereaseMetadata#ViewDef) extends QuereaseViewDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseViewDef()
    private val quereaseExtras = extras(QuereaseViewExtrasKey, defaultExtras)
    val keyFieldNames        = quereaseExtras.keyFieldNames
    override val validations = quereaseExtras.validations
    def updateExtras(updater: QuereaseViewDef => QuereaseViewDef): QuereaseMetadata#ViewDef =
      updateExtras(QuereaseViewExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]) = viewDef.copy(extras = extras)
    override protected def extrasMap = viewDef.extras
  }
  implicit class AugmentedQuereaseFieldDef(fieldDef: QuereaseMetadata#FieldDef) extends QuereaseFieldDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseFieldDef()
    private val quereaseExtras = extras(QuereaseFieldExtrasKey, defaultExtras)
    override val initial = quereaseExtras.initial
    def updateExtras(updater: QuereaseFieldDef => QuereaseFieldDef): QuereaseMetadata#FieldDef =
      updateExtras(QuereaseFieldExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]): Any = fieldDef.copy(extras = extras)
    override protected def extrasMap = fieldDef.extras
  }

  def toQuereaseViewDefs(mojozViewDefs: Map[String, MojozViewDef]): Map[String, MojozViewDef] =
    mojozViewDefs.map(kv => kv._1 -> toQuereaseViewDef(kv._2)).toMap

  def toQuereaseViewDef(viewDef: MojozViewDef): MojozViewDef = {
    import scala.jdk.CollectionConverters._
    val Initial = "initial"
    val Key     = "key"
    val Validations = "validations"
    def getExtraOpt(f: MojozFieldDef, key: String) =
      Option(f.extras).flatMap(_ get key).map {
        case s: String => s
        case i: Int => i.toString
        case l: Long => l.toString
        case d: Double => d.toString
        case bd: BigDecimal => bd.toString
        case b: Boolean => b.toString
        case null => null
        case x => sys.error(
          s"Expecting String, AnyVal, BigDecimal value or no value, viewDef field, key: ${viewDef.name}.${f.name}, $key")
    }
    def getStringSeq(name: String, extras: Map[String, Any]): Seq[String] = {
      getSeq(name, extras) map {
        case s: java.lang.String => s
        case m: java.util.Map[_, _] =>
          if (m.size == 1) m.entrySet.asScala.toList(0).getKey.toString
          else m.toString // TODO error?
        case x => x.toString
      }
    }
    def getSeq(name: String, extras: Map[String, Any]): Seq[_] =
      Option(extras).flatMap(_ get name) match {
        case Some(s: java.lang.String) => Seq(s)
        case Some(a: java.util.ArrayList[_]) => a.asScala.toList
        case None => Nil
        case Some(null) => Seq("")
        case Some(x) => Seq(x)
      }
    val qeFields = viewDef.fields map { f =>
      val initial = getExtraOpt(f, Initial).orNull
      f.updateExtras(_ => QuereaseFieldDef(initial))
    }
    val keyFieldNames = getStringSeq(Key, viewDef.extras).flatMap(
      Option(_).map(_.split(",").filter(_ != "").map(_.trim).toList).getOrElse(Nil))
    val validations = getStringSeq(Validations, viewDef.extras)
    viewDef.copy(fields = qeFields).updateExtras(_ => QuereaseViewDef(keyFieldNames, validations))
  }
}
