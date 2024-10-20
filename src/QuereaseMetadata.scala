package org.mojoz.querease

import com.typesafe.config.{ConfigFactory, ConfigObject, ConfigValueType}
import org.mojoz.metadata._
import org.mojoz.metadata.in.{JoinsParser, YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import org.mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}
import org.tresql.ast.{Ident, Variable}
import org.tresql.{MacroResourcesImpl, OrtMetadata, SimpleCache}
import org.tresql.OrtMetadata._

import java.io.{BufferedReader, InputStream, InputStreamReader}
import scala.collection.immutable.{ListSet, Map, Seq, TreeMap}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import scala.util.Try

case class ViewNotFoundException(message: String) extends Exception(message)
case class FieldOrderingNotFoundException(message: String) extends Exception(message)

trait QuereaseMetadata {
  this: QuereaseExpressions with QuereaseResolvers with QueryStringBuilder
  with QuereaseResolvers with FilterTransformer =>

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

  protected lazy val parserCacheSize: Int = 32 * 1024
  protected lazy val yamlMetadata = YamlMd.fromResources()
  lazy val metadataConventions: MdConventions = new SimplePatternMdConventions(resourceLoader)
  lazy val typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs
  /** db connection name mapping to db instance */
  lazy val aliasToDb: Map[String, String] = QuereaseMetadata.aliasToDb(resourceLoader)
  lazy val tableMetadata: TableMetadata =
    new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions, typeDefs).tableDefs, identity, aliasToDb)
  lazy val macrosClass: Class[_] = classOf[QuereaseMacros]
  protected lazy val resourceLoader: String => InputStream = getClass.getResourceAsStream _
  lazy val joinsParser: JoinsParser = new TresqlJoinsParser(
    TresqlMetadata(tableMetadata.tableDefs, typeDefs, macrosClass, resourceLoader, aliasToDb),
    _ => Some(new SimpleCache(parserCacheSize, "TresqlJoinsParser cache")),
  )
  lazy val uninheritableExtras: Seq[String] = Seq()
  lazy val viewDefLoader: YamlViewDefLoader =
    YamlViewDefLoader(tableMetadata, yamlMetadata, joinsParser, metadataConventions, uninheritableExtras, typeDefs)
  import QuereaseMetadata.toQuereaseViewDefs
  lazy val nameToViewDef: Map[String, ViewDef] = toQuereaseViewDefs {
    viewDefLoader
      .nameToViewDef.asInstanceOf[Map[String, ViewDef]]
  }
  lazy val tresqlMetadata =
    TresqlMetadata(tableMetadata.tableDefs, typeDefs, macrosClass, resourceLoader, aliasToDb, nameToViewDef)

  protected lazy val viewNameToChildViewNames: Map[String, Set[String]] =
    nameToViewDef.collect {
      case (k, v) => k -> v.fields.filter(_.type_.isComplexType).map(_.type_.name).toSet
    }
  protected lazy val childViewNames: Set[String] = viewNameToChildViewNames.values.flatMap(identity).toSet
  protected lazy val viewNameToParentViewNames: Map[String, Set[String]] =
    viewNameToChildViewNames.toSeq.flatMap {
      case (vn, cvnSet) => cvnSet.map(_ -> vn)
    }.groupBy(_._1)
     .map { case (cvn, cvnVnSeq) => cvn -> cvnVnSeq.map(_._2).toSet }
  private[querease] lazy val childViewNameToAncestorDbNames: Map[String, Set[String]] =
    childViewNames.map { childName =>
      val namesQ   = collection.mutable.Queue[String]()
      val namesSet = collection.mutable.Set[String]()
      namesQ ++= viewNameToParentViewNames(childName)
      while (namesQ.nonEmpty) {
        val name = namesQ.dequeue()
        if (!namesSet.contains(name)) {
          namesSet += name
          viewNameToParentViewNames.get(name).foreach(namesQ.++=)
        }
      }
      childName -> namesSet.map(nameToViewDef).map(_.db).toSet
    }.toMap

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
        viewDef.fields
          .filterNot(isOptionalField)
          .map(f => (f.fieldName, if (f.isCollection) Nil else null)))
    }
  lazy val viewNameToKeyFields: Map[String, Seq[FieldDef]] =
    nameToViewDef.map { case (name, viewDef) => (name, keyFields(viewDef)) }
  lazy val viewNameToKeyFieldNames: Map[String, Seq[String]] =
    viewNameToKeyFields.map { case (name, fields) => (name, fields.map(_.fieldName)) }
  lazy val viewNameToKeyFieldTypes: Map[String, Seq[Type]] =
    viewNameToKeyFields.map { case (name, fields) => (name, fields.map(_.type_)) }
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
  def fieldOrderingOptionFromMf[T <: AnyRef: Manifest]: Option[Ordering[String]] = fieldOrderingOption(viewNameFromMf[T])
  def fieldOrderingFromMf[T <: AnyRef](implicit mf: Manifest[T]): Ordering[String] = fieldOrderingOptionFromMf[T]
    .getOrElse(throw FieldOrderingNotFoundException(s"Field ordering for view $mf not found"))
  def viewDefOption(viewName: String): Option[ViewDef] = nameToViewDef.get(viewName)
  def viewDef(viewName: String) = viewDefOption(viewName)
    .getOrElse(throw ViewNotFoundException(s"View definition for $viewName not found"))
  def viewDefOptionFromMf[T <: AnyRef: Manifest]: Option[ViewDef] = viewDefOption(viewNameFromMf[T])
  def viewDefFromMf[T <: AnyRef](implicit mf: Manifest[T]): ViewDef =
    viewDefOptionFromMf[T].getOrElse(throw ViewNotFoundException(s"View definition for type $mf not found"))

  def viewNameFromMf[T <: AnyRef](implicit mf: Manifest[T]): String =
    mf.runtimeClass.getSimpleName

  protected def keyFields(view: ViewDef): Seq[FieldDef] = {
    import QuereaseMetadata.AugmentedQuereaseViewDef
    Option(view.keyFieldNames)
      .filter(_.nonEmpty)
      .map(_.map { fieldNameAndType =>
        val (fieldName, fieldTypeOpt) =
          fieldNameAndType.trim.split("::", 2).toList match {
            case Seq(s)           => (s.trim, None)
            case Seq(s, typeName) => (s.trim, Some(new Type(typeName.trim)))
            case _ => sys.error(s"Failed to parse key for ${view.name}")
          }
        lazy val conventionsType = metadataConventions.typeFromExternal(fieldName, fieldTypeOpt)
        view.fieldOpt(fieldName)
          .orElse(view.table match {
            case null =>
              Some(new FieldDef(fieldName, conventionsType))
            case table =>
              tableMetadata.col(table, fieldName, view.db)
                .map(col => new FieldDef(fieldName, col.type_))
          })
          .map { field => fieldTypeOpt match {
            case None    => field
            case Some(_) => field.copy(type_ = conventionsType)
          }}
          .getOrElse(sys.error(s"Custom key field or column $fieldName not found, view ${view.name}"))
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
    keyColNameOfTypeForGet(view, "long")

  protected def keyColNameForGetByCode(view: ViewDef): String =
    keyColNameOfTypeForGet(view, "string")

  protected def keyColNameOfTypeForGet(view: ViewDef, keyColTypeName: String): String =
    if (view.table != null)
      Option(view.table)
        .map(tableMetadata.tableDef(_, view.db))
        .flatMap { t =>
          ((if (t.pk != null) t.pk.toSeq else Nil) ++
           (if (t.uk != null) t.uk       else Nil)
          ).filter(_ != null)
           .find { k => k.cols.size == 1 && t.cols.exists(col => col.name == k.cols.head && col.type_.name == keyColTypeName) }
        } .map(_.cols.head)
          .orNull
    else if (view.joins != null && view.joins.nonEmpty)
      Option(viewNameToKeyFields(view.name))
        .filter(_.size == 1)
        .flatMap(_.find(_.type_.name == keyColTypeName).map(_.name))
        .orNull
    else null

  val supportedIdTypeNames: Set[String] = ListSet("long", "int", "short")
  protected def idFieldName(view: ViewDef): String =
    tableMetadata.tableDefOption(view).flatMap { t =>
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
      field.table != null         &&
      (!field.type_.isComplexType ||
        viewDefOption(field.type_.name).exists { childView =>
          childView.table == null && (childView.joins == null || childView.joins == Nil)
        }
      ) &&
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
      .map(_.replace("[", "").replace("]", "").replace("?", ""))
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
      .map(_.replace("[", "").replace("]", "").replace("?", ""))
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
  protected def isOptionalField(f: FieldDef): Boolean =
    f.options != null && f.options.contains('?')
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
  protected def persistenceMetadataTypeCast(view: ViewDef, field: FieldDef): String = {
    val type_ =
      if  (field.type_.isComplexType)
           tableMetadata.columnDefOption(view, field).map(_.type_).getOrElse(field.type_)
      else field.type_
    if (field.isCollection) s"::'${type_.name}[]'"
    else if (type_.name == "json") "::json"
    else ""
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
    val keyFields = viewNameToKeyFields(view.name)
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
    val properties = view.fields
      .map { f => try {
        val fieldName = f.fieldName
        def bestLookupRefs = {
          // TODO child multi-tables?
          val childTableName = nameToViewDef.get(f.type_.name).flatMap(saveToNames(_).headOption).orNull
          // TODO searh also reverse refs to avoid runtime exceptions
          // TODO raise more ambiguities, refs search too liberal and thus unstable when db structure changes
          if  (f.isCollection) Nil
          else {
            val refs     = tables.flatMap(_.refs.filter(_.refTable == childTableName))
            val bestRefs = refs.filter(r => f.saveTo != null && f.saveTo == s"${r.refTable}:${r.cols.head}")
            val goodRefs = if (bestRefs.isEmpty) refs.filter(_.defaultRefTableAlias == fieldName) else Nil
            (if (bestRefs.nonEmpty) bestRefs else if (goodRefs.nonEmpty) goodRefs else refs).toSeq
          }
        }
        def persistencePropertyValue(valueTresql: String) = {
          val tresqlValue = TresqlValue(
            tresql    = valueTresql,
          )
          if (keyFields.contains(f)) {
            val where = s"if_defined_or_else($oldKeyRef.$fieldName?, $oldKeyRef.$fieldName?, :$fieldName)"
            if (tables.exists(_.pk.exists(_.cols.contains(fieldName))))
              KeyValue(
                whereTresql = where,
                valueTresql = AutoValue(valueTresql),
                updValueTresql = Some(AutoValue(valueTresql)),
              )
            else
              KeyValue(
                whereTresql = where,
                valueTresql = tresqlValue,
              )
          } else tresqlValue
        }
        val childView =
          if (f.type_.isComplexType) {
            val childViewName = f.type_.name
            val childView = nameToViewDef.getOrElse(childViewName,
              sys.error(s"View $childViewName referenced from ${view.name} not found")
            )
            childView
          } else null
        lazy val isPk = view.table != null &&
          tableMetadata.tableDefOption(view).flatMap(_.pk).map(_.cols).contains(Seq(f.name))
        lazy val forInsert = isFieldForInsert(f)
        lazy val forUpdate = isFieldForUpdate(f) && !isPk
        if (isSaveableField_(f)) {
          val saveTo    = Option(f.saveTo).getOrElse(f.name)
          def typeCast  = persistenceMetadataTypeCast(view, f)
          val valueTresql =
            if (f.saveTo == null && f.resolver == null)
              s":${fieldName}${if (isOptionalField(f)) "?" else ""}${typeCast}"
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
            optional = isOptionalField(f),
            forInsert = forInsert,
            forUpdate = forUpdate,
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
                optional = isOptionalField(f),
                forInsert = forInsert,
                forUpdate = forUpdate,
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
          .map { childPersistenceMetadata =>
            bestLookupRefs match {
              case Nil =>
                Property(
                  col   = f.name,
                  value = ViewValue(childPersistenceMetadata, childSaveOptions),
                  optional = isOptionalField(f),
                  forInsert = forInsert,
                  forUpdate = forUpdate,
                )
              case Seq(ref) =>
                // TODO prefer single-col ref, throw for multi-col
                val refColName = ref.cols.head
                Property(
                  col   = refColName,
                  value = LookupViewValue(
                    fieldName,
                    childPersistenceMetadata,
                  ),
                  optional = isOptionalField(f),
                  forInsert = forInsert,
                  forUpdate = forUpdate,
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

  import QueryStringBuilder.CompilationUnit
  /** All queries and dml-s from viewDef for compilation, together with group name - to test viewDef */
  def allQueryStrings(viewDef: ViewDef): Seq[CompilationUnit] = {
    if (viewDef.fields != null && viewDef.fields.nonEmpty &&
      (viewDef.table != null || viewDef.joins != null && viewDef.joins.nonEmpty))
      List(
        CompilationUnit("queries", viewDef.name, viewDef.db, queryStringAndParams(viewDef, Map.empty)._1)
      )
    else Nil
  } ++ validationsQueryStrings(viewDef).map(vq => CompilationUnit("validations", viewDef.name, viewDef.db, vq))

  protected def generateQueriesForCompilation(log: => String => Unit): Seq[CompilationUnit] = {
    val viewsToCompile =
      nameToViewDef.values.toList
      .filter(viewDef => !childViewNames.contains(viewDef.name)) // compile only top-level views
      .sortBy(_.name)
    log(s"Generating queries to be compiled for ${viewsToCompile.size} top-level views" +
        s" (${nameToViewDef.size} views total)")
    val startTime = System.currentTimeMillis
    val result = viewsToCompile flatMap allQueryStrings
    val endTime = System.currentTimeMillis
    log(s"Query generation done in ${endTime - startTime} ms, ${result.size} queries generated")
    result
  }

  protected def compileQueries(
    category: String,
    compilationUnits: Seq[CompilationUnit],
    previouslyCompiledQueries: Set[String],
    showFailedViewQuery: Boolean,
    log: => String => Unit,
  ): Int = {
    log(s"Compiling $category - ${compilationUnits.size} total")
    val startTime = System.currentTimeMillis
    val dbToCompiler = compilationUnits.map(_.db).toSet.map { (db: String) => db -> new org.tresql.compiling.Compiler {
      override val metadata = if (db == null) tresqlMetadata else tresqlMetadata.extraDbToMetadata(db)
      override val extraMetadata = tresqlMetadata.extraDbToMetadata
      override protected val macros =
        new MacroResourcesImpl(Option(macrosClass).map(_.getDeclaredConstructor().newInstance()).orNull, tresqlMetadata)
    }}.toMap
    val compiledQueries = collection.mutable.Set[String](previouslyCompiledQueries.toSeq: _*)
    var compiledCount = 0
    compilationUnits.foreach { case cu @ CompilationUnit(_, viewName, db, q) =>
      if (!compiledQueries.contains(cu.queryStringWithContext)) {
        val compiler = dbToCompiler(db)
        try compiler.compile(compiler.parseExp(q)) catch { case util.control.NonFatal(ex) =>
          val msg = s"\nFailed to compile $viewName query: ${ex.getMessage}" +
            (if (showFailedViewQuery) s"\n$q" else "")
          throw new RuntimeException(msg, ex)
        }
        compiledCount += 1
        compiledQueries += cu.queryStringWithContext
      }
    }
    val endTime = System.currentTimeMillis
    val allQueries = compilationUnits.map(_.queryStringWithContext).toSet
    log(
      s"Compilation done - $category - ${endTime - startTime} ms, " +
      s"queries compiled: $compiledCount" +
      (if (compiledCount != allQueries.size) s" of ${allQueries.size}" else ""))
    compiledCount
  }

  /** Clear all caches used for query compilation */
  protected def clearAllCaches(): Unit = {
    joinsParser match {
      case tjp: TresqlJoinsParser =>
        tjp.dbToCompilerAndCache.values.foreach(_._2.foreach(_.clear()))
      case _ =>
    }
    parser.cache.foreach(_.clear())
  }

  /** Compile all queries - to test viewDef. Used by sbt-mojoz plugin */
  def compileAllQueries(
    previouslyCompiledQueries: Set[String],
    showFailedViewQuery: Boolean,
    log: => String => Unit,
  ): (Set[String], Map[String, Array[Byte]]) = {
    val startTime = System.currentTimeMillis
    if (previouslyCompiledQueries.isEmpty)
      clearAllCaches() // clear caches if full recompile required, e.g. when table or type metadata changed
    val queriesForCompilation = generateQueriesForCompilation(log)
    val categorizedQueriesForCompilation = queriesForCompilation.groupBy(_.category).toSeq.sortBy(_._1)
    val compiledQueries = collection.mutable.Set[String](previouslyCompiledQueries.toSeq: _*)
    val compiledCount =
      categorizedQueriesForCompilation.map { case (category, compilationUnits) =>
        val count =
          compileQueries(category, compilationUnits, compiledQueries.toSet, showFailedViewQuery, log)
        compiledQueries ++= compilationUnits.map(_.queryStringWithContext)
        count
      }.sum
    val allQueries = queriesForCompilation.map(_.queryStringWithContext).toSet
    val endTime = System.currentTimeMillis
    log(
      s"View compilation done in ${endTime - startTime} ms, " +
      s"queries compiled: $compiledCount" +
      (if (compiledCount != allQueries.size) s" of ${allQueries.size}" else ""))
    (allQueries, serializedCaches)
  }

  protected def serializedCaches: Map[String, Array[Byte]] = Map.empty
}

object QuereaseMetadata {

  trait QuereaseViewDefExtras {
    val keyFieldNames: Seq[String]
    val minSearchKeyFieldCount: Int
    val validations: Seq[String]
  }

  private [querease] case class QuereaseViewDef(
    keyFieldNames: Seq[String] = Nil,
    minSearchKeyFieldCount: Int = 0,
    validations: Seq[String] = Nil
  ) extends QuereaseViewDefExtras

  trait QuereaseFieldDefExtras {
    val initial: String
  }

  private [querease] case class QuereaseFieldDef(
    initial: String = null
  ) extends QuereaseFieldDefExtras

  val BindVarCursorsCmd = "build cursors"
  val BindVarCursorsForViewCmd = "build cursors for view"
  val BindVarCursorsForViewCmdRegex =
    new Regex(BindVarCursorsForViewCmd + """\s+(\w+)""")

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
  implicit class AugmentedQuereaseViewDef(viewDef: ViewDef) extends QuereaseViewDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseViewDef()
    private val quereaseExtras = extras(QuereaseViewExtrasKey, defaultExtras)
    override val keyFieldNames = quereaseExtras.keyFieldNames
    override val minSearchKeyFieldCount = quereaseExtras.minSearchKeyFieldCount
    override val validations = quereaseExtras.validations
    def updateExtras(updater: QuereaseViewDef => QuereaseViewDef): ViewDef =
      updateExtras(QuereaseViewExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]) = viewDef.copy(extras = extras)
    override protected def extrasMap = viewDef.extras
  }
  implicit class AugmentedQuereaseFieldDef(fieldDef: FieldDef) extends QuereaseFieldDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseFieldDef()
    private val quereaseExtras = extras(QuereaseFieldExtrasKey, defaultExtras)
    override val initial = quereaseExtras.initial
    def updateExtras(updater: QuereaseFieldDef => QuereaseFieldDef): FieldDef =
      updateExtras(QuereaseFieldExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]): Any = fieldDef.copy(extras = extras)
    override protected def extrasMap = fieldDef.extras
  }

  def aliasToDb(resourceLoader: String => InputStream): Map[String, String] = {
    val confOpt =
     Option(resourceLoader("/tresql-resources.conf"))
      .map(inputStream => new BufferedReader(new InputStreamReader(inputStream)))
      .map(ConfigFactory parseReader _)
      .filter(_ hasPath "tresql")

    val defaultCpName =
     confOpt
      .map(_ -> "tresql.default")
      .filter { case (c, n) => c hasPathOrNull n }
      .map { case (c, n) => if (c getIsNull n) null else c.getString(n) }
      .getOrElse("main")

    val defaultDbNameOpt =
     confOpt
      .map(_ -> "tresql.db")
      .filter { case (c, n) => c hasPathOrNull n }
      .map { case (c, n) => if (c getIsNull n) null else c.getString(n) }

    confOpt
      .map(_ getConfig "tresql")
      .toSeq
      .flatMap(_.root().asScala
        .collect { case e@(_, v) if v.valueType() == ConfigValueType.OBJECT => e }
        .flatMap { case (cpName, confValue) =>
          val aliases  = if (cpName == defaultCpName) Seq(null, defaultCpName) else Seq(cpName)
          val dbName =
            Option(confValue).collect {
              case confObj: ConfigObject => confObj.toConfig
            }.filter(_.hasPathOrNull("db"))
             .map(conf => if (conf.getIsNull("db")) null else conf.getString("db"))
             .orElse(defaultDbNameOpt)
             .getOrElse(aliases.head)
          aliases.map(_ -> dbName)
        }
      )
      .filter { case (alias, dbName) => alias != dbName }
      .toMap
  }

  def toQuereaseViewDefs(mojozViewDefs: Map[String, ViewDef]): Map[String, ViewDef] =
    mojozViewDefs.map(kv => kv._1 -> toQuereaseViewDef(kv._2)).toMap

  private val orderByParser: QuereaseExpressions.DefaultParser = new QuereaseExpressions.DefaultParser(None)

  def toQuereaseViewDef(viewDef: ViewDef): ViewDef = {
    import scala.jdk.CollectionConverters._
    val Initial = "initial"
    val Key     = "key"
    val Validations = "validations"
    def getExtraOpt(f: FieldDef, key: String) =
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
    val qeFields = viewDef.fields.map { f =>
      val initial = getExtraOpt(f, Initial).orNull
      f.updateExtras(_ => QuereaseFieldDef(initial))
    }.map { f =>
      if (f.expression != null && f.expression.indexOf("#") > 0) {
        val (exp, ord) = orderByParser.parseWithParser(orderByParser.colAndOrd)(f.expression)
        if (ord != null)
          f.copy(
            expression = exp.tresql,
            orderBy    = (Option(f.orderBy).toSeq ++ ord.cols.map(_.tresql)).mkString(", "),
          )
        else f
      } else f
    }
    val rawKey = getStringSeq(Key, viewDef.extras).flatMap(Option(_).toList).mkString(",")
    val keyFieldNames =
      rawKey.split("[,()]+").map(_.trim).filter(_ != "").toList
    val minSearchKeyFieldCount =
      rawKey.indexOf('(') match {
        case -1 => keyFieldNames.size
        case ix => rawKey.substring(0, ix).split("[,()]+").map(_.trim).filter(_ != "").size
      }
    val validations = getStringSeq(Validations, viewDef.extras)
    viewDef.copy(fields = qeFields).updateExtras(_ =>
      QuereaseViewDef(keyFieldNames, minSearchKeyFieldCount, validations))
  }
}
