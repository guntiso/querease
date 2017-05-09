package querease

import scala.collection.immutable.TreeMap
import scala.collection.JavaConversions._
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try

import org.tresql.Column
import org.tresql.Result
import org.tresql.RowLike

import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.io.MdConventions
import mojoz.metadata.io.SimplePatternMdConventions
import mojoz.metadata.out.ScalaClassWriter
import mojoz.metadata.TableDef.TableDefBase
import mojoz.metadata.ColumnDef.ColumnDefBase

import org.tresql.QueryParser
import org.tresql.QueryParser.Ident
import org.tresql.QueryParser.Variable

trait ScalaDtoQuereaseIo extends QuereaseIo { this: Querease =>

  override type DTO <: Dto

  override def fromRows[B <: DTO](rows: Result[RowLike])(implicit mf: Manifest[B]): List[B] = {
    def toDto(r: RowLike): B = {
      val t = mf.runtimeClass.newInstance
      t.asInstanceOf[B].fill(r)
    }
    rows.map(toDto).toList
  }
  override def toSaveableMap[B <: DTO: Manifest](instance: B) = instance.toSaveableMap
  override def keyMap[B <: DTO: Manifest](instance: B) = instance match {
    case o: DtoWithId @unchecked => Map("id" -> o.id)
    case x => sys.error( // TODO use viewDef to get key-values if defined
      s"getting key map for ${instance.getClass.getName} not supported yet")
  }

  protected def rowLikeToDto[T <: Dto](r: RowLike, m: Manifest[T]): T =
    m.runtimeClass.newInstance.asInstanceOf[T].fill(r)

  trait Dto {

    //filling in object from RowLike
    private val _setters: Map[String, (java.lang.reflect.Method, (Manifest[_], Manifest[_ <: Dto]))] =
      (for (
        m <- getClass.getMethods;
        if m.getName.endsWith("_$eq") && m.getParameterTypes.length == 1
      ) yield {
        val name = m.getName.dropRight(4)
        name -> (m -> manifest(name, m.getParameterTypes()(0)))
      }).toMap
    //end filling in object from RowLike
    def fill(r: RowLike): this.type = {
      for (i <- 0 to (r.columnCount - 1)) r.column(i) match {
        case Column(_, name, _) if name != null => set(name, r)
        case _ =>
      }
      this
    }
    protected def setters = _setters
    protected def set(dbName: String, r: RowLike) =
      (for (s <- setters.get(dbToPropName(dbName))) yield {
        if (s._2._2 != null) { //child result
          val m: Manifest[_ <: Dto] = s._2._2
          val childResult = r.result(dbName)
          s._1.invoke(this, childResult.list[Dto](rowLikeToDto _,
            m.asInstanceOf[Manifest[Dto]]).asInstanceOf[Object])
        } else if (classOf[Dto].isAssignableFrom(s._2._1.runtimeClass)) { // single child result
          val m: Manifest[_ <: Dto] = s._2._1.asInstanceOf[Manifest[_ <: Dto]]
          val childResult = r.result(dbName)
          s._1.invoke(this, childResult.list[Dto](rowLikeToDto _,
            m.asInstanceOf[Manifest[Dto]]).headOption.orNull.asInstanceOf[Object])
        } else s._1.invoke(this, r.typed(dbName)(s._2._1).asInstanceOf[Object])
        s /*return setter used*/
      }) getOrElse {
        setExtras(dbName, r)
      }
    protected def setExtras(dbName: String, r: RowLike): AnyRef = {
      throw new RuntimeException(s"Setter for '$dbName' not found")
    }
    protected def dbToPropName(name: String) = name // .toLowerCase
    protected def propToDbName(name: String) = name
    protected def manifest(name: String, clazz: Class[_]) =
      if (!clazz.isPrimitive)
        ManifestFactory.classType(clazz) ->
          (if (classOf[Seq[_]].isAssignableFrom(clazz) && clazz.isAssignableFrom(classOf[List[_]])) { //get child type
            childManifest(getClass.getMethods.filter(_.getName == name).head.getGenericReturnType)
          } else null)
      else (clazz match {
        case java.lang.Integer.TYPE => ManifestFactory.Int
        case java.lang.Long.TYPE => ManifestFactory.Long
        case java.lang.Double.TYPE => ManifestFactory.Double
        case java.lang.Boolean.TYPE => ManifestFactory.Boolean
      }) -> null
    protected def childManifest(t: java.lang.reflect.Type): Manifest[_ <: Dto] = {
      val parametrisedType = t.asInstanceOf[java.lang.reflect.ParameterizedType]
      val clazz = parametrisedType.getActualTypeArguments()(0) match {
        case c: Class[_] => c
        case v: java.lang.reflect.TypeVariable[_] =>
          v.getGenericDeclaration.asInstanceOf[Class[_]]
      }
      ManifestFactory.classType(clazz)
    }

    def toMap: Map[String, Any] =
      toMap(fieldOrdering(ManifestFactory.classType(getClass)))
    def toMap(fieldOrdering: Ordering[String]): Map[String, Any] =
      TreeMap[String, Any]()(fieldOrdering) ++
      (setters.flatMap { m =>
      scala.util.Try(getClass.getMethod(m._1).invoke(this)).toOption.map {
        case s: Seq[_] => m._1 ->  s.map{
          case dto: Dto => dto.toMap
          case str: String => str
          case i: java.lang.Integer => i
        }
        case c: Dto => m._1 -> c.toMap
        case x => m._1 -> x
      }
    } toMap)

    def containsField(fieldName: String) = setters.contains(fieldName)

    override def toString: String = {
      val view = viewDef(ManifestFactory.classType(getClass))
      val fieldNames = view.fields.map { f => Option(f.alias) getOrElse f.name }
      toString(fieldNames)
    }

    protected def toString(fieldNames: Seq[String]): String = {
      def isMisleading(s: String) =
        s.contains(" ") || s == "null" || s == "" || s(0).isDigit
      val kv: Seq[(String, Object)] = fieldNames.flatMap{ name =>
        Try(getClass.getMethod(name)).map(_.invoke(this) match {
          case s: Seq[_] => name ->
            ("(" + (s map {
              case s: String => "\"" + s + "\""
              case o => o.toString
            }).mkString(", ") + ")")
          case s: String if isMisleading(s) => name -> ("\"" + s + "\"")
          case x => name -> x
        }).map(List(_)).toOption.getOrElse(Nil)
      }
      s"${getClass.getName}{${kv.map(kv => kv._1 + ": " + kv._2).mkString(", ")}"
    }

    private[querease] val IdentifierPatternString = "([\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*\\.)*[\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*"
    private[querease] val IdentifierExtractor = s"($IdentifierPatternString).*".r

    protected def isSavableField(field: FieldDef, view: ViewDef, saveToMulti: Boolean, saveToTableNames: Seq[String]) =
      (!field.isExpression &&
        !field.type_.isComplexType &&
        field.table != null &&
        (!saveToMulti &&
          field.table == view.table &&
          (field.tableAlias == null || field.tableAlias == view.tableAlias)
          ||
          saveToMulti &&
          saveToTableNames.contains(field.table) // TODO alias?
          )
       ||
       field.saveTo != null
     )

    //creating map from object
    def toSaveableMap: Map[String, Any] = {
      // FIXME child handling, do we rely on metadata or method list?
      val view = viewDef(ManifestFactory.classType(getClass))
      val saveToMulti = view.saveTo != null && view.saveTo.size > 0
      val saveTo =
        if (!saveToMulti) Seq(view.table)
        else view.saveTo
      def identifier(s: String) = s match {
        case IdentifierExtractor(ident, _) => ident
        case _ => ""
      }
      val saveToTableNames = saveTo.map(identifier)
      def isForInsert = this match {
        case o: DtoWithId => o.id
        case _ => sys.error(s"isForInsert() for ${getClass.getName} not supported yet") // TODO isForInsert
      }
      def isForUpdate = this match {
        case o: DtoWithId => o.id
        case _ => sys.error(s"isForUpdate() for ${getClass.getName} not supported yet") // TODO isForUpdate
      }
      def isSaveableField(field: FieldDef) =
        isSavableField(field, view, saveToMulti, saveToTableNames)
      def tablesTo(v: ViewDef) =
        if (v.saveTo != null && v.saveTo.size > 0)
          v.saveTo.mkString("#")
        else if (v.table != null)
          v.table
        else
          null
      def isChildTableField(field: FieldDef) =
        !field.isExpression &&
          field.type_.isComplexType &&
          viewDefOption(field.type_.name).map(tablesTo).orNull != null
      def saveableKeys(f: FieldDef): Seq[String] = {
        // TODO various mixes of options and params etc?
        val name = f.name
        def alias = Option(f.alias).getOrElse(f.name)
        if (f.saveTo == null && f.resolver == null)
          Seq(name + Option(f.options).getOrElse(""))
        else {
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
          def impliedRefResolvers(f: FieldDef, refFieldDef: FieldDef) = {
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
                    .getOrElse(impliedRefResolvers(f, refFieldDef))
              }
          val resolvers =
            explicitResolvers(f)
              .orElse(referencedResolvers)
              .getOrElse(impliedResolvers(f, true))
          if (resolvers.size == 0) {
            throw new RuntimeException(
              s"Failed to imply resolver for ${view.name}.$alias")
          }
          val fSaveTo = Option(f.saveTo) getOrElse name
          resolvers.map(alias + "->" + fSaveTo + "=" + _) ++ Seq(alias + "->")
        }
      }
      val keysValues = (setters.toList.flatMap { m =>
        val methodName = m._1
        val fieldName = propToDbName(methodName)
        scala.util.Try(getClass.getMethod(methodName).invoke(this)).toOption.map {
          case Nil => view.fields
            .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
            .filter(isChildTableField)
            .map(f => viewDefOption(f.type_.name).map(v => tablesTo(v) + f.options).get -> Nil)
            .orElse(Some("*" + methodName -> Nil)) // prefix * to avoid clashes
            .toList
          case s: Seq[_] =>
            val options = view.fields
              .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
              .filter(isChildTableField)
              .map(_.options)
              .headOption getOrElse ""
            //objects from one list can be put into different tables
            s.asInstanceOf[Seq[Dto]] map { d =>
              (tablesTo(viewDef(ManifestFactory.classType(d.getClass))) + options, d.toSaveableMap)
            } groupBy (_._1) map (t => t.copy(_2 = t._2.map(_._2))) toList
          case d: Dto => view.fields
            .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
            .filter(isChildTableField)
            .map { f =>
              val tables = saveToTableNames.map(tableMetadata.tableDef)
              val childTableName = viewDefOption(f.type_.name).map(tablesTo).get
              val key = // XXX too complicated to save a child
                // TODO support multiple, multiple direction, multi-col etc. refs properly
                tables
                  .zipWithIndex
                  .map(ti => (
                    ti._1.name != f.table, ti._2, // for priority (sorting)
                    ti._1, ti._1.refs.count(_.refTable == childTableName)))
                  .sortBy(x => "" + x._1 + x._2)
                  .map(x => x._3 -> x._4)
                  .filter(_._2 > 0)
                  .headOption // <--- FIXME what's this BS?
                  .map {
                    case (table, 1) => // TODO multi-col?
                      table.refs.find(_.refTable == childTableName).map(_.cols.head).get
                    case (table, n) => // FIXME analyze aliases and/or joins?
                      f.name + "_id"
                  }.getOrElse(childTableName)
              key + f.options -> d.toSaveableMap
            }
            .orElse(Some("*" + methodName -> d.toSaveableMap)) // prefix * to avoid clashes
            .toList
          case x => view.fields
            .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
            .filter(isSaveableField)
            .map(f => saveableKeys(f).map(_ -> x))
            .orElse(Some(List("*" + methodName -> x))) // prefix * to avoid clashes
            .toList.flatten
        } getOrElse Nil
      })
      //child objects from different lists can be put into one table
      keysValues groupBy { case (_, _: Seq[_]) => "s" case _ => "v" } flatMap {
        case ("s", seq) =>
          seq.asInstanceOf[Seq[(String, Seq[_])]] groupBy (_._1) map {
            t => t.copy(_2 = t._2.flatMap(_._2))
          }
        case (_, kv) => kv
      }
    }
  }

  trait DtoWithId extends Dto {
    def id: java.lang.Long
    def id_=(id: java.lang.Long)
  }
}
