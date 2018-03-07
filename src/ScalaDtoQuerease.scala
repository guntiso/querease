package querease

import scala.collection.immutable.TreeMap
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try

import org.tresql.Column
import org.tresql.RowLike


trait ScalaDtoQuereaseIo extends QuereaseIo with QuereaseResolvers { this: Querease =>

  override type DTO >: Null <: Dto
  type DWI <: DTO with DtoWithId

  override def convertRow[B <: DTO](row: RowLike)(implicit mf: Manifest[B]): B = rowLikeToDto(row, mf)

  override def toSaveableMap[B <: DTO](dto: B) =
    withProxy(_.proxyToSaveableMap(dto))(ManifestFactory.classType(dto.getClass))

  override def keyMap[B <: DTO](dto: B) = dto match {
    case o: DtoWithId => Map("id" -> o.id)
    case _ => sys.error( // TODO use viewDef to get key-values if defined
      s"getting key map for ${dto.getClass.getName} not supported yet")
  }

  def toMap[B <: DTO](dto: B) = {
    val mf = ManifestFactory.classType(dto.getClass)
    val map = withProxy(_.toUnorderedMap(dto))(mf)
    fieldOrderingOption(mf).map { fo => TreeMap[String, Any]()(fo) ++ map }.getOrElse(map)
  }

  /** org.tresql.Converter[B] */
  implicit def rowLikeToDto[B <: DTO](r: RowLike, m: Manifest[B]): B =
    withProxy(_.fill(m.runtimeClass.newInstance.asInstanceOf[B], r))(m)

  def toString[B <: DTO](dto: B) = withProxy(_.proxyToString(dto))(ManifestFactory.classType(dto.getClass))

  protected def withProxy[A, B <: DTO: Manifest](action: Proxy => A) =
    proxies.get(implicitly[Manifest[B]]).map(action)
      .getOrElse(sys.error(s"Proxy not found for manifest ${implicitly[Manifest[B]]}"))

  def viewNameToClassMap: Map[String, Class[_ <: DTO]]
  def classToViewNameMap: Map[Class[_], String] = viewNameToClassMap.map(_.swap)

  protected lazy val proxies: Map[Manifest[_ <: DTO], Proxy] = viewDefs.map { case (n, v) =>
    ManifestFactory.classType(viewNameToClassMap(n)) -> new Proxy(v)
  }

  /** Ensures viewDef corresponding Dto instance transformation i.e. filling from tresql row, converting to map and saveable map */
  class Proxy(viewDef: ViewDef) {
    protected val clazz: Class[_ <: DTO] = viewNameToClassMap(viewDef.name)
    protected val setters: Map[String, (java.lang.reflect.Method, (Manifest[_], Manifest[_ <: DTO]))] = {
      def manifest(name: String, clazz: Class[_]) =
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
      def childManifest(t: java.lang.reflect.Type): Manifest[_ <: DTO] = {
        val parametrisedType = t.asInstanceOf[java.lang.reflect.ParameterizedType]
        val clazz = parametrisedType.getActualTypeArguments()(0) match {
          case c: Class[_] => c
          case v: java.lang.reflect.TypeVariable[_] =>
            v.getGenericDeclaration.asInstanceOf[Class[_]]
        }
        ManifestFactory.classType(clazz)
      }
      (for (
        m <- clazz.getMethods
        if m.getName.endsWith("_$eq") && m.getParameterTypes.length == 1
      ) yield {
        val name = m.getName.dropRight(4)
        name -> (m -> manifest(name, m.getParameterTypes()(0)))
      }).toMap
    }

    protected def dbToPropName(name: String) = name // .toLowerCase
    protected def propToDbName(name: String) = name

    protected[querease] def toUnorderedMap(dto: DTO): Map[String, Any] = setters.flatMap { m =>
      scala.util.Try(clazz.getMethod(m._1).invoke(dto)).toOption.map {
        case s: Seq[_] => m._1 -> s.map {
          case dto: Dto => toMap(dto.asInstanceOf[DTO])
          case str: String => str
          case i: java.lang.Integer => i
        }
        case dto: Dto => m._1 -> toMap(dto.asInstanceOf[DTO])
        case x => m._1 -> x
      }
    }

    def containsField(fieldName: String) = setters.contains(fieldName)

    def proxyToString(dto: DTO): String = {
      val fieldNames = viewDef.fields.map { f => Option(f.alias) getOrElse f.name }
      def isMisleading(s: String) =
        s.contains(" ") || s == "null" || s == "" || s(0).isDigit
      val kv: Seq[(String, Object)] = fieldNames.flatMap { name =>
        Try(clazz.getMethod(name)).map(_.invoke(this) match {
          case s: Seq[_] => name ->
            ("(" + (s map {
              case s: String => "\"" + s + "\""
              case dto: Dto => ScalaDtoQuereaseIo.this.toString(dto.asInstanceOf[DTO])
              case o => o.toString
            }).mkString(", ") + ")")
          case s: String if isMisleading(s) => name -> ("\"" + s + "\"")
          case dto: Dto => name -> ScalaDtoQuereaseIo.this.toString(dto.asInstanceOf[DTO])
          case x => name -> x
        }).map(List(_)).toOption.getOrElse(Nil)
      }
      s"${getClass.getName}{${kv.map(kv => kv._1 + ": " + kv._2).mkString(", ")}"
    }

    private[querease] val IdentifierPatternString = "([\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*\\.)*[\\p{IsLatin}_$][\\p{IsLatin}\\d_$]*"
    private[querease] val IdentifierExtractor = s"($IdentifierPatternString).*".r
    def identifier(s: String) = s match {
      case IdentifierExtractor(ident, _) => ident
      case _ => ""
    }
    protected def isSavableField(field: FieldDef,
                                 view: ViewDef,
                                 saveToMulti: Boolean,
                                 saveToTableNames: Seq[String]) =
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

    protected lazy val saveableValue: String => PartialFunction[Any, List[(String, Any)]] = {
      val view = viewDef
      val saveToMulti = view.saveTo != null && view.saveTo.nonEmpty
      val saveTo = if (!saveToMulti) Seq(view.table) else view.saveTo
      val saveToTableNames = saveTo.map(identifier)
      // FIXME child handling, do we rely on metadata or method list?
      def isForInsert = this match {
        case o: DtoWithId => o.id == null
        case _ => sys.error(s"isForInsert() for ${getClass.getName} not supported yet") // TODO isForInsert
      }
      def isForUpdate = this match {
        case o: DtoWithId => o.id != null
        case _ => sys.error(s"isForUpdate() for ${getClass.getName} not supported yet") // TODO isForUpdate
      }
      def isSaveableField(field: QuereaseMetadata#FieldDef) =
        isSavableField(field, view, saveToMulti, saveToTableNames)
      def tablesTo(v: QuereaseMetadata#ViewDef) =
        if (v.saveTo != null && v.saveTo.nonEmpty)
          v.saveTo.mkString("#")
        else if (v.table != null)
          v.table
        else
          null
      def isChildTableField(field: QuereaseMetadata#FieldDef) =
        !field.isExpression &&
          field.type_.isComplexType &&
          viewDefOption(field.type_.name).map(tablesTo).orNull != null
      def saveableEntries(f: QuereaseMetadata#FieldDef, v: Any): Seq[(String, Any)] = {
        // TODO various mixes of options and params etc?
        val name = f.name
        def alias = Option(f.alias).getOrElse(f.name)
        if (f.saveTo == null && f.resolver == null)
          Seq(name + Option(f.options).getOrElse("") -> v)
        else {
          val resolvers = allResolvers(view, f)
          if (resolvers.isEmpty) {
            throw new RuntimeException(
              s"Failed to imply resolver for ${view.name}.$alias")
          }
          val fSaveTo = Option(f.saveTo) getOrElse name
          resolvers
            .map(r => (alias + "->") -> (fSaveTo + "=" + r)) ++ Seq(alias -> v)
        }
      }
      propName => {
        val fieldName = propToDbName(propName)
        val saveableValueFunc: PartialFunction[Any, List[(String, Any)]] = {
          case Nil => view.fields
            .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
            .filter(isChildTableField)
            .map(f => viewDefOption(f.type_.name).map(v => tablesTo(v) + f.options).get -> Nil)
            .orElse(Some("*" + propName -> Nil)) // prefix * to avoid clashes
            .toList
          case s: Seq[_] =>
            val options = view.fields
              .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
              .filter(isChildTableField)
              .map(_.options) getOrElse ""
            //objects from one list can be put into different tables
            s.asInstanceOf[Seq[DTO]] map { d =>
              (tablesTo(ScalaDtoQuereaseIo.this.viewDef(ManifestFactory.classType(d.getClass))) + options, toSaveableMap(d))
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
                  .map(x => x._3 -> x._4).find(_._2 > 0) // <--- FIXME what's this BS?
                  .map {
                  case (table, 1) => // TODO multi-col?
                    table.refs.find(_.refTable == childTableName).map(_.cols.head).get
                  case (table, n) => // FIXME analyze aliases and/or joins?
                    f.name + "_id"
                }.getOrElse(childTableName)
              key + f.options -> toSaveableMap(d.asInstanceOf[DTO])
            }
            .orElse(Some("*" + propName -> toSaveableMap(d.asInstanceOf[DTO]))) // prefix * to avoid clashes
            .toList
          case x => view.fields
            .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
            .filter(isSaveableField)
            .map(f => saveableEntries(f, x))
            .orElse(Some(List("*" + propName -> x))) // prefix * to avoid clashes
            .toList.flatten
        }
        saveableValueFunc
      }
    }
    /** Creating saveable map from object for passing to tresql ort */
    def proxyToSaveableMap(dto: DTO): Map[String, Any] = setters.toList.flatMap { m =>
      val propName = m._1
      scala.util.Try(getClass.getMethod(propName).invoke(dto)).toOption.map {
        saveableValue(propName)(_)
      } getOrElse Nil
    }.groupBy { case (_, _: Seq[_]) => "s" case _ => "v" } //child objects from different lists can be put into one table
      .flatMap {
      case ("s", seq) =>
        seq.asInstanceOf[Seq[(String, Seq[_])]] groupBy (_._1) map {
          t => t.copy(_2 = t._2.flatMap(_._2))
        }
      case (_, kv) => kv
    }

    /** Fill object from row like */
    def fill[T <: DTO](dto: T, r: RowLike): T = {
      def set(dbName: String, r: RowLike) =
        (for (s <- setters.get(dbToPropName(dbName))) yield {
          if (s._2._2 != null) { //child result
            val m: Manifest[_ <: DTO] = s._2._2
            val childResult = r.result(dbName)
            s._1.invoke(dto, childResult.list[DTO](rowLikeToDto,
              m.asInstanceOf[Manifest[DTO]]).asInstanceOf[Object])
          } else if (classOf[Dto].isAssignableFrom(s._2._1.runtimeClass)) { // single child result
            val m: Manifest[_ <: DTO] = s._2._1.asInstanceOf[Manifest[_ <: DTO]]
            val childResult = r.result(dbName)
            s._1.invoke(dto, childResult.list[DTO](rowLikeToDto,
              m.asInstanceOf[Manifest[DTO]]).headOption.orNull.asInstanceOf[Object])
          } else s._1.invoke(dto, r.typed(dbName)(s._2._1).asInstanceOf[Object])
          s /*return setter used*/
        }) getOrElse {
          setExtras(dto, dbName, r)
        }
      for (i <- 0 until r.columnCount) r.column(i) match {
        case Column(_, name, _) if name != null => set(name, r)
        case _ =>
      }
      dto
    }
    /** Invoked if coresponding setter of row column {{{dbName}}} is not found */
    protected def setExtras(dto: DTO, dbName: String, r: RowLike): AnyRef = {
      throw new RuntimeException(s"Setter for '$dbName' not found")
    }
  }
}

trait Dto
trait DtoWithId extends Dto {
  def id: java.lang.Long
  def id_=(id: java.lang.Long)
}
