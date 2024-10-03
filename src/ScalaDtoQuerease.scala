package org.mojoz.querease

import scala.collection.immutable.{TreeMap, Seq}
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try

import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentHashMap

import org.mojoz.metadata.{FieldDef, ViewDef}
import org.tresql.Column
import org.tresql.CoreTypes
import org.tresql.RowLike


class ScalaDtoQuereaseIo[-DTO <: Dto](qe: QuereaseMetadata with QuereaseResolvers with ValueTransformer) extends QuereaseIo[DTO] {
  override def convertRow[B <: DTO](row: RowLike)(implicit mf: Manifest[B]): B =
    rowLikeToDto(row, mf)
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
  def fill(r: RowLike)(implicit qe: QuereaseMetadata with ValueTransformer): this.type = {
    for (i <- 0 until r.columnCount) r.column(i) match {
      case c if c.name != null => set(c.name, c.isResult, r)
      case _ =>
    }
    this
  }

  protected def setters = DtoReflection.setters(this)
  protected def set(dbName: String, isResult: Boolean, r: RowLike)(implicit qe: QuereaseMetadata with ValueTransformer) =
    (for (s <- setters.get(dbToPropName(dbName))) yield {
      def newDto(mDto: Manifest[_ <: Dto]): QDto =
        mDto.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[QDto]
      //declare local converter
      def conv[A <: QDto]: CoreTypes.Converter[A] =
        (r, m) => m.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[A].fill(r)
      val value =
        (s.mfSeq, s.mfDto) match {
          case (null, null) =>
            r.typed(dbName)(s.mfOth).asInstanceOf[Object]
          case (null, mDto) =>
            if (isResult) {
              val childResult = r.result(dbName)
              childResult.list[QDto](mDto.asInstanceOf[Manifest[QDto]], conv).headOption.orNull.asInstanceOf[Object]
            } else {
              val view = qe.viewDefFromMf(mDto)
              qe.toCompatibleMap(r(dbName), view, dbName) match {
                case null => null
                case map  => newDto(mDto).fill(map)
              }
            }
          case (mSeq, null) =>
            if (isResult)
              r.result(dbName).map(_.typed(0)(s.mfOth)).toList.asInstanceOf[Object]
            else
              r(dbName) match {
                case arr: java.sql.Array =>
                  import org.tresql._
                  import org.tresql.given
                  val tresqlResult: Result[RowLike] = arr.getResultSet
                  val value = tresqlResult.map(_.typed(1)(s.mfOth)).toList.asInstanceOf[Object]
                  arr.free()
                  value
                case x =>
                  List(r.typed(dbName)(s.mfOth)).asInstanceOf[Object]
              }
          case (mSeq, mDto) =>
            if (isResult) {
              val childrenResult = r.result(dbName)
              childrenResult.list[QDto](mDto.asInstanceOf[Manifest[QDto]], conv).asInstanceOf[Object]
            } else {
              val view = qe.viewDefFromMf(mDto)
              qe.toCompatibleSeqOfMaps(r(dbName), view, dbName) match {
                case null => null
                case maps =>
                  maps.map { map =>
                    newDto(mDto).fill(map)
                  }
              }
            }
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
  def toMap(implicit qe: QuereaseMetadata): Map[String, Any] =
    qe.fieldOrderingOptionFromMf(ManifestFactory.classType(getClass))
      .map(toMapWithOrdering).getOrElse(toUnorderedMap)
  def toMapWithOrdering(fieldOrdering: Ordering[String])(implicit qe: QuereaseMetadata): Map[String, Any] =
    TreeMap[String, Any]()(fieldOrdering) ++ toUnorderedMap

  def containsField(fieldName: String) = setters.contains(fieldName)

  def toString(implicit qe: QuereaseMetadata): String = {
    val view = qe.viewDefFromMf(ManifestFactory.classType(getClass))
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

  protected def throwUnsupportedConversion(
      value: Any, targetType: Manifest[_], fieldName: String, cause: Throwable = null): Unit = {
    throw new RuntimeException(
      s"Illegal value or unsupported type conversion from ${value.getClass.getName}" +
      s" to ${targetType.toString} - failed to populate ${getClass.getName}.$fieldName", cause)
  }

  //creating dto from Map[String, Any]
  def fill(values: Map[String, Any])(implicit qe: QuereaseMetadata with ValueTransformer): this.type = fill(values, emptyStringsToNull = true)(qe)
  def fill(values: Map[String, Any], emptyStringsToNull: Boolean)(implicit qe: QuereaseMetadata with ValueTransformer): this.type = {
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
            // TODO wrap, unwrap seqs!
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
              } else {
                if  (s.nonEmpty && mOth != null)
                     s.map(qe.convertToType(_, mOth.runtimeClass))
                else s
              }
            } else
              throwUnsupportedConversion(value, Option(mDto).getOrElse(mOth), name)
          case null => null
          case x if mOth != null => qe.convertToType(x, mOth.runtimeClass)
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
