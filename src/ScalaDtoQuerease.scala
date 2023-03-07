package org.mojoz.querease

import scala.collection.immutable.{TreeMap, Seq}
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try

import java.util.concurrent.ConcurrentHashMap

import org.mojoz.metadata.{FieldDef, ViewDef}
import org.tresql.Column
import org.tresql.RowLike


class ScalaDtoQuereaseIo[-DTO <: Dto](qe: QuereaseMetadata with QuereaseResolvers) extends QuereaseIo[DTO] {
  override def convertRow[B <: DTO](row: RowLike)(implicit mf: Manifest[B]): B =
    rowLikeToDto(row, mf)
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  def toSaveableMap[B <: DTO](dto: B): Map[String, Any] =
    dto.toSaveableMap(qe)
  @deprecated("Results of this method are not used and this method will be removed", "6.1.0")
  def keyMap[B <: DTO](dto: B) = dto match {
    case o: DtoWithId => Map("id" -> o.id)
    case _ => sys.error(
      s"getting key map for ${dto.getClass.getName} not supported yet")
  }
  override def toMap[B <: DTO](dto: B): Map[String, Any] =
    dto.toMap(qe)

  //org.tresql.Converter[T]
  implicit def rowLikeToDto[B <: DTO](r: RowLike, m: Manifest[B]): B =
    m.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[B].fill(r)(qe)
}

case class DtoSetter(
  name: String,
  method: java.lang.reflect.Method,
  mfOpt: Manifest[_ <: Option[_]],
  mfSeq: Manifest[_ <: Seq[_]],
  mfDto: Manifest[_ <: Dto],
  mfOth: Manifest[_],
)

private[querease] object DtoReflection {
  private val cache =
    new ConcurrentHashMap[Class[_ <: Dto], Map[String, DtoSetter]]
  def manifest(name: String, dto: Dto, clazz: Class[_]): DtoSetter = {
    def mOpt(clazz: Class[_]): Manifest[_ <: Option[_]] = {
      if (classOf[Option[_]].isAssignableFrom(clazz))
        ManifestFactory.classType(clazz)
      else null
    }
    def mSeq(clazz: Class[_]): Manifest[_ <: Seq[_]] = {
      if (classOf[Seq[_]].isAssignableFrom(clazz) && clazz.isAssignableFrom(classOf[List[_]]))
        ManifestFactory.classType(clazz)
      else null
    }
    def mDto(clazz: Class[_]): Manifest[_ <: Dto] = {
      if (classOf[Dto].isAssignableFrom(clazz))
        ManifestFactory.classType(clazz)
      else null
    }
    def mOth(clazz: Class[_]): Manifest[_] = {
      if (clazz.isPrimitive) clazz match {
        case java.lang.Integer.TYPE => ManifestFactory.Int
        case java.lang.Long.TYPE    => ManifestFactory.Long
        case java.lang.Double.TYPE  => ManifestFactory.Double
        case java.lang.Boolean.TYPE => ManifestFactory.Boolean
      } else ManifestFactory.classType(clazz)
    }
    lazy val childMf =
      childManifest(dto.getClass.getMethods.filter(_.getName == name).head.getGenericReturnType)
    lazy val grandChildMf =
      childManifest(dto.getClass.getMethods.filter(_.getName == name).head.getGenericReturnType, 2)
    val mfOpt = mOpt(clazz)
    val mfSeq = mfOpt match {
      case null => mSeq(clazz)
      case mOpt => mSeq(childMf.runtimeClass)
    }
    val mfDto = (mfOpt, mfSeq) match {
      case (null, null) => mDto(clazz)
      case (_ ,   null) => mDto(childMf.runtimeClass)
      case (null,    _) => mDto(childMf.runtimeClass)
      case _            => mDto(grandChildMf.runtimeClass)
    }
    val mfOth = (mfOpt, mfSeq, mfDto) match {
      case (null, null, null) => mOth(clazz)
      case (_,    null, null) => mOth(childMf.runtimeClass)
      case (null,    _, null) => mOth(childMf.runtimeClass)
      case (_,       _, null) => mOth(grandChildMf.runtimeClass)
      case _ => null
    }
    DtoSetter(name, null, mfOpt, mfSeq, mfDto, mfOth)
  }
  def childManifest(t: java.lang.reflect.Type, depth: Int = 1): Manifest[_] = {
    val parametrisedType = t.asInstanceOf[java.lang.reflect.ParameterizedType]
    depth match {
      case 1 =>
        val clazz = parametrisedType.getActualTypeArguments()(0) match {
          case c: Class[_] => c
          case p: java.lang.reflect.ParameterizedType =>
            p.getRawType.asInstanceOf[Class[_]]
          case v: java.lang.reflect.TypeVariable[_] =>
            v.getGenericDeclaration.asInstanceOf[Class[_]]
        }
        ManifestFactory.classType(clazz)
      case 2 =>
        parametrisedType.getActualTypeArguments()(0) match {
          case p: java.lang.reflect.ParameterizedType =>
            p.getActualTypeArguments()(0) match {
              case cc: Class[_] => ManifestFactory.classType(cc)
            }
        }
    }
  }
  def setters(dto: Dto): Map[String, DtoSetter] = {
    val dtoClass = dto.getClass
    cache.getOrDefault(dtoClass, {
      val setters =
        (for (
          m <- dtoClass.getMethods
          if m.getName.endsWith("_$eq") &&
            !java.lang.reflect.Modifier.isFinal(m.getModifiers) &&
            m.getParameterTypes.length == 1
        ) yield {
          val name = m.getName.dropRight(4)
          name -> manifest(name, dto, m.getParameterTypes()(0)).copy(method = m)
        }).toMap
      cache.putIfAbsent(dtoClass, setters)
      setters
    })
  }
}

trait Dto { self =>

  protected type QDto >: Null <: Dto

  // populate from RowLike
  def fill(r: RowLike)(implicit qe: QuereaseMetadata): this.type = {
    for (i <- 0 until r.columnCount) r.column(i) match {
      case Column(_, name, _) if name != null => set(name, r)
      case _ =>
    }
    this
  }

  protected def setters = DtoReflection.setters(this)
  protected def set(dbName: String, r: RowLike)(implicit qe: QuereaseMetadata) =
    (for (s <- setters.get(dbToPropName(dbName))) yield {
      //declare local converter
      def conv[A <: QDto](r: RowLike, m: Manifest[A]): A = m.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[A].fill(r)
      val value =
        (s.mfSeq, s.mfDto) match {
          case (null, null) =>
            r.typed(dbName)(s.mfOth).asInstanceOf[Object]
          case (null, mDto) =>
            val childResult = r.result(dbName)
            childResult.list[QDto](conv, mDto.asInstanceOf[Manifest[QDto]]).headOption.orNull.asInstanceOf[Object]
          case (mSeq, null) =>
            r.typed(dbName)(mSeq).asInstanceOf[Object]
          case (mSeq, mDto) =>
            val childrenResult = r.result(dbName)
            childrenResult.list[QDto](conv, mDto.asInstanceOf[Manifest[QDto]]).asInstanceOf[Object]
        }
      if  (s.mfOpt == null)
           s.method.invoke(this, value)
      else s.method invoke(this, Some(value))
      s /*return setter used*/
    }) getOrElse {
      setExtras(dbName, r)
    }
  protected def setExtras(dbName: String, r: RowLike)(implicit qe: QuereaseMetadata): AnyRef = {
    // Exception disabled because DynamicResult leaks synthetic helper columns - ignoring unknown columns
    // throw new RuntimeException(s"Setter for '$dbName' not found")
    null: AnyRef
  }
  protected def dbToPropName(name: String) = name // .toLowerCase
  protected def propToDbName(name: String) = name
  protected def manifest(name: String, clazz: Class[_]) =
    DtoReflection.manifest(name, this, clazz)
  protected def childManifest(t: java.lang.reflect.Type, depth: Int = 1): Manifest[_] = {
    DtoReflection.childManifest(t, depth)
  }

  def toUnorderedMap(implicit qe: QuereaseMetadata): Map[String, Any] = setters.map { m =>
    val propName = m._1
    val propValue = getClass.getMethod(propName).invoke(this) match {
      case s: Seq[_] => s.map {
        case dto: Dto => dto.asInstanceOf[QDto].toMap
        case x => x
      }
      case c: Dto => c.asInstanceOf[QDto].toMap
      case Some(x) => x match {
        case s: Seq[_] => s.map {
          case dto: Dto => dto.asInstanceOf[QDto].toMap
          case x => x
        }
        case c: Dto => c.asInstanceOf[QDto].toMap
        case x => x
      }
      case x => x
    }
    propName -> propValue
  }.filter(_._2 != None)
  def toMap(implicit qe: QuereaseMetadata): Map[String, Any] = qe.fieldOrderingOption(ManifestFactory.classType(getClass))
    .map(toMapWithOrdering).getOrElse(toUnorderedMap)
  def toMapWithOrdering(fieldOrdering: Ordering[String])(implicit qe: QuereaseMetadata): Map[String, Any] =
    TreeMap[String, Any]()(fieldOrdering) ++ toUnorderedMap

  def containsField(fieldName: String) = setters.contains(fieldName)

  def toString(implicit qe: QuereaseMetadata): String = {
    val view = qe.viewDef(ManifestFactory.classType(getClass))
    val fieldNames = view.fields.map(_.fieldName)
    toString(fieldNames)
  }

  protected def toString(fieldNames: Seq[String])(implicit qe: QuereaseMetadata): String = {
    def isMisleading(s: String) =
      s.contains(" ") || s == "null" || s == "" || s(0).isDigit
    val kv: Seq[(String, Object)] = fieldNames.map { name =>
      getClass.getMethod(name).invoke(this) match {
        case s: Seq[_] => name ->
          ("(" + (s map {
            case s: String => "\"" + s + "\""
            case d: Dto => d.asInstanceOf[QDto].toString
            case o => o.toString
          }).mkString(", ") + ")")
        case s: String if isMisleading(s) => name -> ("\"" + s + "\"")
        case d: Dto => name -> d.asInstanceOf[QDto].toString
        case x => name -> x
      }
    }
    s"${getClass.getName}{${kv.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
  }

  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  protected def identifier(s: String) = s match {
    case QuereaseExpressions.IdentifierExtractor(ident, _) => ident
    case _ => ""
  }
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
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
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  protected def isSavableChildField(field: FieldDef,
                                    view: ViewDef,
                                    saveToMulti: Boolean,
                                    saveToTableNames: Seq[String],
                                    childView: ViewDef): Boolean =
    true
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  protected lazy val saveableValue: QuereaseMetadata with QuereaseResolvers => String => PartialFunction[Any, List[(String, Any)]] = implicit qe => {
    val view = qe.viewDef(ManifestFactory.classType(getClass))
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
    def isSaveableField(field: FieldDef) =
      isSavableField(field, view, saveToMulti, saveToTableNames)
    def isSaveableChildField(field: FieldDef, childView: ViewDef) =
      isSavableChildField(field, view, saveToMulti, saveToTableNames, childView)
    def ortDbPrefix(db: String): String =
      Option(db).map(db => s"@$db:") getOrElse ""
    def withOrtDbPrefix(db: String)(t: String): String =
      if (t startsWith "@") t else ortDbPrefix(db) + t
    def tablesTo(v: ViewDef) =
      if (v.saveTo != null && v.saveTo.nonEmpty)
        v.saveTo.map(withOrtDbPrefix(v.db)).mkString("#")
      else if (v.saveTo == Nil)
        null
      else if (v.table != null)
        withOrtDbPrefix(v.db)(v.table)
      else
        null
    def childSaveTo(field: FieldDef, childView: ViewDef) =
      Option(field).map(_.saveTo).filter(_ != null) getOrElse tablesTo(childView)
    def isChildTableField(field: FieldDef) =
      !field.isExpression &&
        field.type_.isComplexType &&
        qe.viewDefOption(field.type_.name).map(tablesTo).orNull != null
    def fieldOptionsRef(field: FieldDef) =
      Option(field.options)
        .map(_.replace("[", "").replace("]", ""))
        .map(_.split("/", 2))
        .map { arr => if (arr.length == 0) null else arr(arr.length - 1) }
        .filter(_ != "")
        .orNull
    def saveableEntries(f: FieldDef, v: Any): Seq[(String, Any)] = {
      // TODO various mixes of options and params etc?
      val name = f.name
      def alias = f.fieldName
      if (f.saveTo == null && f.resolver == null)
        Seq(name + Option(fieldOptionsRef(f)).getOrElse("") -> v)
      else {
        val resolvers = qe.allResolvers(view, f)
        if (resolvers.isEmpty) {
          throw new RuntimeException(
            s"Failed to imply resolver for ${view.name}.$alias")
        }
        val fSaveTo = Option(f.saveTo) getOrElse name
        val resolvedVal =
          //if complex type encountered, call toMap since values may be used in resolvers.
          if (v != null && f.type_.isComplexType && !f.isCollection) v.asInstanceOf[QDto].toMap else v
        resolvers
          .map(r => (alias + "->") -> (fSaveTo + "=" + r)) ++ Seq(alias -> resolvedVal)
      }
    }
    propName => {
      val fieldName = propToDbName(propName)
      val propFieldDefOpt = view.fieldOpt(fieldName)
      val childTableFieldDefOpt = propFieldDefOpt
        .filter(isChildTableField)
      def toSaveable(x: Any) = x match {
        case dto: Dto => dto.asInstanceOf[QDto].toSaveableMap
        case simple   => x
      }
      val saveableValueFunc: PartialFunction[Any, List[(String, Any)]] = {
        case Nil => childTableFieldDefOpt
          .flatMap(f => qe.viewDefOption(f.type_.name)
            .map(v => v -> childSaveTo(f, v))
            .filter(_._2 != null)
            .filter(vs => isSaveableChildField(f, vs._1))
            .map(_._2)
            .map(_ + Option(fieldOptionsRef(f)).getOrElse("") -> Nil))
          .orElse(Some("*" + propName -> Nil)) // prefix * to avoid clashes
          .toList
        case s: Seq[_] =>
          val options = childTableFieldDefOpt
            .map(fieldOptionsRef).filter(_ != null) getOrElse ""
          val f = childTableFieldDefOpt.orNull
          //objects from one list can be put into different tables
          s map { d =>
            qe.viewDefOption(ManifestFactory.classType(d.getClass))
              .map(v => v -> childSaveTo(f, v))
              .filter(_._2 != null)
              .filter(vs => isSaveableChildField(f, vs._1))
              .map(_._2)
              .map(_ + options -> toSaveable(d))
              .getOrElse("*" + propName -> toSaveable(d)) // prefix * to avoid clashes
          } groupBy (_._1) map (t => t.copy(_2 = t._2.map(_._2))) toList
        case d if childTableFieldDefOpt.isDefined =>
          val value = Option(d).map(toSaveable).orNull
          childTableFieldDefOpt
          .filter(f => isSaveableChildField(f, qe.viewDefOption(f.type_.name).get))
          .map { f =>
            val tables = saveToTableNames.map(qe.tableMetadata.tableDef(_, view.db))
            val childTableName = qe.viewDefOption(f.type_.name).map(tablesTo).get
            val key = // XXX too complicated to save a child
            // TODO support multiple, multiple direction, multi-col etc. refs properly
              Option(f.saveTo)
                .getOrElse {
                  tables
                    .sorted( // sort is stable
                      Ordering.by((table: org.mojoz.metadata.TableDef) =>
                        if (table.name == f.table) 0 else 1))
                    .find { table => table.refs.exists(_.refTable == childTableName) }
                    .map { table => table -> table.refs.count(_.refTable == childTableName) }
                    .map {
                      case (table, 1) => // TODO multi-col?
                        table.refs.find(_.refTable == childTableName).map(_.cols.head).get
                      case (table, n) => // FIXME analyze aliases and/or joins?
                        f.name + "_id"
                    }.getOrElse(childTableName)
                }
            key + Option(fieldOptionsRef(f)).getOrElse("") -> value
          }
          .orElse(Some("*" + propName -> value)) // prefix * to avoid clashes
          .toList
        case x => propFieldDefOpt
          .filter(isSaveableField)
          .map(f => saveableEntries(f, x))
          .orElse(Some(List("*" + propName -> x))) // prefix * to avoid clashes
          .toList.flatten
      }
      saveableValueFunc
    }
  }
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  def toSaveableMap(implicit qe: QuereaseMetadata with QuereaseResolvers): Map[String, Any] = setters.toList.flatMap { m =>
    val propName = m._1
    Try(getClass.getMethod(propName).invoke(this)).toOption.map {
      saveableValue(qe)(propName)(_)
    } getOrElse Nil
  }.groupBy { case (_, _: Seq[_]) => "s" case _ => "v" } //child objects from different lists can be put into one table
    .flatMap {
    case ("s", seq) =>
      seq.asInstanceOf[Seq[(String, Seq[_])]] groupBy (_._1) map {
        t => t.copy(_2 = t._2.flatMap(_._2))
      }
    case (_, kv) => kv
  }

  protected def throwUnsupportedConversion(
      value: Any, targetType: Manifest[_], fieldName: String, cause: Throwable = null): Unit = {
    throw new RuntimeException(
      s"Illegal value or unsupported type conversion from ${value.getClass.getName}" +
      s" to ${targetType.toString} - failed to populate ${getClass.getName}.$fieldName", cause)
  }

  //creating dto from Map[String, Any]
  def fill(values: Map[String, Any])(implicit qe: QuereaseMetadata): this.type = fill(values, emptyStringsToNull = true)(qe)
  def fill(values: Map[String, Any], emptyStringsToNull: Boolean)(implicit qe: QuereaseMetadata): this.type = {
    values foreach { case (name, value) =>
      setters.get(name).map { case DtoSetter(_, met, mOpt, mSeq, mDto, mOth) =>
        val converted = value match {
          case s: String if emptyStringsToNull && s.trim == "" => null
          case m: Map[String@unchecked, _] =>
            if (mSeq == null && mDto != null)
              mDto.runtimeClass.getConstructor().newInstance().asInstanceOf[QDto].fill(m, emptyStringsToNull)
            else
              throwUnsupportedConversion(value, Option(mSeq).getOrElse(mOth), name)
          case s: Seq[_] =>
            if (mSeq != null) {
              val c = mSeq.runtimeClass
              val isList = c.isAssignableFrom(classOf[List[_]])
              val isVector = c.isAssignableFrom(classOf[Vector[_]])
              if (classOf[Seq[_]].isAssignableFrom(c) && (isList || isVector) && mDto != null) {
                val chClass = mDto.runtimeClass
                val res =
                  s.map(o => chClass.getConstructor().newInstance().asInstanceOf[QDto]
                    .fill(o.asInstanceOf[Map[String, Any]], emptyStringsToNull))
                if (isList) res.toList else res
              } else s
            } else
              throwUnsupportedConversion(value, Option(mDto).getOrElse(mOth), name)
          case x => x
        }
        val convertedObj =
          if  (mOpt == null) converted else Some(converted)
        try met.invoke(this, convertedObj.asInstanceOf[Object]) catch {
          case util.control.NonFatal(ex) =>
            throwUnsupportedConversion(value, Seq(mSeq, mDto, mOth).filter(_ != null).head, name)
        }
      }
    }
    this
  }
}

trait DtoWithId extends Dto {
  def id: java.lang.Long
  def id_=(id: java.lang.Long): Unit
}
