package org.mojoz.querease

import com.typesafe.config.{Config, ConfigFactory}
import org.mojoz.metadata.Type
import org.mojoz.metadata.{FieldDef, ViewDef}
import org.mojoz.metadata.in.{Join, JoinsParser}
import org.mojoz.querease.QuereaseMetadata.{BindVarCursorsCmd, BindVarCursorsForViewCmd, BindVarCursorsForViewCmdRegex}
import org.snakeyaml.engine.v2.api.{Dump, DumpSettings}
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import org.snakeyaml.engine.v2.common.{FlowStyle, ScalarStyle}
import org.tresql.ast.{Arr, Exp, Ident, Null, With, Query => QueryParser_Query}
import org.tresql.{Cache, Column, InsertResult, ORT, OrtMetadata, Query, Resources, Result, RowLike, UpdateResult}

import java.lang.StringBuilder
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime, ZoneId}
import java.util.Base64
import scala.annotation.tailrec
import scala.collection.immutable.{Map, Seq, Set}
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try
import scala.util.control.NonFatal

class NotFoundException(msg: String) extends Exception(msg)
class ValidationException(msg: String, val details: List[ValidationResult]) extends Exception(msg)
case class ValidationResult(path: List[Any], messages: List[String])

object SaveMethod extends Enumeration {
  type SaveMethod = Value
  val Save, Insert, Update, Upsert = Value
}
import org.mojoz.querease.SaveMethod._

trait QuereaseIteratorResult[+B] extends Iterator[B] with AutoCloseable {
  def view: ViewDef
}

trait FieldFilter {
  /** If field is optional and this method returns false then the field is excluded from query */
  def shouldInclude(field: String): Boolean
  def childFilter(field: String): FieldFilter
}

object ValueConverter {

  val ClassOfJavaLangBoolean        = classOf[java.lang.Boolean]
  val ClassOfJavaLangDouble         = classOf[java.lang.Double]
  val ClassOfJavaLangInteger        = classOf[java.lang.Integer]
  val ClassOfJavaLangLong           = classOf[java.lang.Long]
  val ClassOfJavaLangObject         = classOf[java.lang.Object]
  val ClassOfJavaMathBigDecimal     = classOf[java.math.BigDecimal]
  val ClassOfJavaMathBigInteger     = classOf[java.math.BigInteger]
  val ClassOfJavaSqlDate            = classOf[java.sql.Date]
  val ClassOfJavaSqlTime            = classOf[java.sql.Time]
  val ClassOfJavaSqlTimestamp       = classOf[java.sql.Timestamp]
  val ClassOfJavaTimeInstant        = classOf[Instant]
  val ClassOfJavaTimeLocalDate      = classOf[LocalDate]
  val ClassOfJavaTimeLocalDateTime  = classOf[LocalDateTime]
  val ClassOfJavaTimeLocalTime      = classOf[LocalTime]
  val ClassOfJavaTimeOffsetDateTime = classOf[OffsetDateTime]
  val ClassOfJavaTimeZonedDateTime  = classOf[ZonedDateTime]
  val ClassOfJavaUtilDate           = classOf[java.util.Date]
  val ClassOfScalaMathBigDecimal    = classOf[scala.math.BigDecimal]
  val ClassOfScalaMathBigInt        = classOf[scala.math.BigInt]
  val ClassOfString                 = classOf[java.lang.String]

  val ClassOfBoolean                = classOf[Boolean]
  val ClassOfDouble                 = classOf[Double]
  val ClassOfInt                    = classOf[Int]
  val ClassOfLong                   = classOf[Long]
  val ClassOfShort                  = classOf[Short]

  val ClassOfByteArray              = classOf[Array[Byte]]

  val supportedClassNameToClass: Map[String, Class[_]] =
    Map(
      ClassOfJavaLangBoolean         .getName -> ClassOfJavaLangBoolean,
      ClassOfJavaLangDouble          .getName -> ClassOfJavaLangDouble,
      ClassOfJavaLangInteger         .getName -> ClassOfJavaLangInteger,
      ClassOfJavaLangLong            .getName -> ClassOfJavaLangLong,
      ClassOfJavaLangObject          .getName -> ClassOfJavaLangObject,
      ClassOfJavaMathBigDecimal      .getName -> ClassOfJavaMathBigDecimal,
      ClassOfJavaMathBigInteger      .getName -> ClassOfJavaMathBigInteger,
      ClassOfJavaSqlDate             .getName -> ClassOfJavaSqlDate,
      ClassOfJavaSqlTime             .getName -> ClassOfJavaSqlTime,
      ClassOfJavaSqlTimestamp        .getName -> ClassOfJavaSqlTimestamp,
      ClassOfJavaTimeInstant         .getName -> ClassOfJavaTimeInstant,
      ClassOfJavaTimeLocalDate       .getName -> ClassOfJavaTimeLocalDate,
      ClassOfJavaTimeLocalDateTime   .getName -> ClassOfJavaTimeLocalDateTime,
      ClassOfJavaTimeLocalTime       .getName -> ClassOfJavaTimeLocalTime,
      ClassOfJavaTimeOffsetDateTime  .getName -> ClassOfJavaTimeOffsetDateTime,
      ClassOfJavaTimeZonedDateTime   .getName -> ClassOfJavaTimeZonedDateTime,
      ClassOfJavaUtilDate            .getName -> ClassOfJavaUtilDate,
      ClassOfScalaMathBigDecimal     .getName -> ClassOfScalaMathBigDecimal,
      ClassOfScalaMathBigInt         .getName -> ClassOfScalaMathBigInt,
      ClassOfString                  .getName -> ClassOfString,
      //
      ClassOfBoolean                 .getName -> ClassOfBoolean,
      ClassOfDouble                  .getName -> ClassOfDouble,
      ClassOfInt                     .getName -> ClassOfInt,
      ClassOfLong                    .getName -> ClassOfLong,
      ClassOfShort                   .getName -> ClassOfShort,
      //
      ClassOfByteArray               .getName -> ClassOfByteArray,
      //
      "String"                                -> ClassOfString,
      "Boolean"                               -> ClassOfBoolean,
      "Double"                                -> ClassOfDouble,
      "Int"                                   -> ClassOfInt,
      "Long"                                  -> ClassOfLong,
      "Short"                                 -> ClassOfShort,
      "Array[Byte]"                           -> ClassOfByteArray,
    )
}

trait ValueConverter {
  import ValueConverter._
  protected def throwUnsupportedConversion(value: Any, targetClass: Class[_]): Unit =
    throw new RuntimeException(
      s"Unsupported type conversion from ${
        Option(value).map(_.getClass.getName).orNull} to ${Option(targetClass).map(_.getName).orNull}")

  protected lazy val config: Config = ConfigFactory.load

  protected lazy val zoneId: ZoneId =
    if (config.hasPath("data.timezone"))
         ZoneId.of(config.getString("data.timezone"))
    else ZoneId.systemDefault

  /* Converts primitive values */
  def convertToType(value: Any, targetClass: Class[_]): Any = value match {
    case null                             => null
    case x if targetClass == ClassOfJavaLangObject ||
              targetClass == x.getClass   => value
    case b: Boolean                   => targetClass match {
      case ClassOfJavaLangBoolean         => b
      case ClassOfString                  => b.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case b: java.lang.Boolean         => targetClass match {
      case ClassOfBoolean                 => b
      case ClassOfString                  => b.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case n: java.lang.Number          => targetClass match {
      case ClassOfDouble                  => n.doubleValue
      case ClassOfInt                     => n.intValue
      case ClassOfJavaLangDouble          => n.doubleValue
      case ClassOfJavaLangInteger         => n.intValue
      case ClassOfJavaLangLong            => n.longValue
      case ClassOfJavaMathBigDecimal      => new java.math.BigDecimal(n.toString) // TODO optimize?
      case ClassOfJavaMathBigInteger      => new java.math.BigInteger(n.toString) // TODO optimize?
      case ClassOfLong                    => n.longValue
      case ClassOfScalaMathBigDecimal     => scala.math.BigDecimal(n.toString)    // TODO optimize?
      case ClassOfScalaMathBigInt         => scala.math.BigInt(n.toString)        // TODO optimize?
      case ClassOfShort                   => n.shortValue
      case ClassOfString                  => n.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case s: java.lang.String          => targetClass match {
      case ClassOfBoolean                 => s.toBoolean
      case ClassOfByteArray               => Base64.getDecoder.decode(s)
      case ClassOfDouble                  => if (s == "") null else s.toDouble
      case ClassOfInt                     => if (s == "") null else s.toInt
      case ClassOfJavaLangBoolean         => s.toBoolean
      case ClassOfJavaLangDouble          => if (s == "") null else s.toDouble
      case ClassOfJavaLangInteger         => if (s == "") null else s.toInt
      case ClassOfJavaLangLong            => if (s == "") null else s.toLong
      case ClassOfJavaMathBigDecimal      => new java.math.BigDecimal(s)
      case ClassOfJavaSqlDate             => java.sql.Date.valueOf(s)
      case ClassOfJavaSqlTime             => java.sql.Time.valueOf(java.time.LocalTime.parse(s))
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(s.replace('T', ' '))
      case ClassOfJavaTimeInstant         => Instant.parse(s)
      case ClassOfJavaTimeLocalDate       => LocalDate.parse(s)
      case ClassOfJavaTimeLocalDateTime   => LocalDateTime.parse(s.replace(' ', 'T'))
      case ClassOfJavaTimeLocalTime       => LocalTime.parse(s)
      case ClassOfJavaTimeOffsetDateTime  => OffsetDateTime.parse(s.replace(' ', 'T'))
      case ClassOfJavaTimeZonedDateTime   => ZonedDateTime.parse(s.replace(' ', 'T'))
      case ClassOfJavaUtilDate            => java.util.Date.from(LocalDate.parse(s).atStartOfDay(ZoneId.systemDefault()).toInstant())
      case ClassOfLong                    => if (s == "") null else s.toLong
      case ClassOfScalaMathBigDecimal     => if (s == "") null else scala.math.BigDecimal(s)
      case ClassOfScalaMathBigInt         => if (s == "") null else scala.math.BigInt(s)
      case ClassOfShort                   => if (s == "") null else s.toLong
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case a: Array[Byte]               => targetClass match {
      case ClassOfString                  => Base64.getEncoder.encodeToString(a)
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.sql.Date             => targetClass match {
      case ClassOfJavaTimeLocalDate       => x.toLocalDate
      case ClassOfJavaUtilDate            => java.util.Date.from(x.toLocalDate.atStartOfDay.atZone(zoneId).toInstant)
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.sql.Time             => targetClass match {
      case ClassOfJavaTimeLocalTime       => x.toLocalTime
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.sql.Timestamp        => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date.valueOf(x.toLocalDateTime.toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time.valueOf(x.toLocalDateTime.toLocalTime)
      case ClassOfJavaTimeInstant         => x.toLocalDateTime.atZone(zoneId).toInstant
      case ClassOfJavaTimeLocalDate       => x.toLocalDateTime.toLocalDate
      case ClassOfJavaTimeLocalDateTime   => x.toLocalDateTime
      case ClassOfJavaTimeLocalTime       => x.toLocalDateTime.toLocalTime
      case ClassOfJavaTimeOffsetDateTime  => x.toLocalDateTime.atZone(zoneId).toOffsetDateTime
      case ClassOfJavaTimeZonedDateTime   => x.toLocalDateTime.atZone(zoneId)
      case ClassOfJavaUtilDate            => java.util.Date.from(x.toLocalDateTime.atZone(zoneId).toInstant)
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.LocalDate       => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x.atStartOfDay)
      case ClassOfJavaTimeInstant         => x.atStartOfDay.atZone(zoneId).toInstant
      case ClassOfJavaTimeLocalDateTime   => x.atStartOfDay
      case ClassOfJavaTimeOffsetDateTime  => x.atStartOfDay.atZone(zoneId).toOffsetDateTime
      case ClassOfJavaTimeZonedDateTime   => x.atStartOfDay.atZone(zoneId)
      case ClassOfJavaUtilDate            => java.util.Date.from(x.atStartOfDay.atZone(zoneId).toInstant)
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.LocalTime       => targetClass match {
      case ClassOfJavaSqlTime             => java.sql.Time.valueOf(x)
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.LocalDateTime   => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x.toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time     .valueOf(x.toLocalTime)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x)
      case ClassOfJavaTimeInstant         => x.atZone(zoneId).toInstant
      case ClassOfJavaTimeLocalDate       => x.toLocalDate
      case ClassOfJavaTimeLocalTime       => x.toLocalTime
      case ClassOfJavaTimeOffsetDateTime  => x.atZone(zoneId).toOffsetDateTime
      case ClassOfJavaTimeZonedDateTime   => x.atZone(zoneId)
      case ClassOfJavaUtilDate            => java.util.Date.from(x.atZone(zoneId).toInstant)
      case ClassOfString                  => java.sql.Timestamp.valueOf(x).toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.Instant         => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x.atZone(zoneId).toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time     .valueOf(x.atZone(zoneId).toLocalTime)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x.atZone(zoneId).toLocalDateTime)
      case ClassOfJavaTimeLocalDate       => x.atZone(zoneId).toLocalDate
      case ClassOfJavaTimeLocalDateTime   => x.atZone(zoneId).toLocalDateTime
      case ClassOfJavaTimeLocalTime       => x.atZone(zoneId).toLocalTime
      case ClassOfJavaTimeOffsetDateTime  => x.atZone(zoneId).toOffsetDateTime
      case ClassOfJavaTimeZonedDateTime   => x.atZone(zoneId)
      case ClassOfJavaUtilDate            => java.util.Date.from(x)
      case ClassOfString                  => x.toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.OffsetDateTime  => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x.toInstant.atZone(zoneId).toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time     .valueOf(x.toInstant.atZone(zoneId).toLocalTime)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x.toInstant.atZone(zoneId).toLocalDateTime)
      case ClassOfJavaTimeInstant         => x.toInstant
      case ClassOfJavaTimeLocalDate       => x.toInstant.atZone(zoneId).toLocalDate
      case ClassOfJavaTimeLocalDateTime   => x.toInstant.atZone(zoneId).toLocalDateTime
      case ClassOfJavaTimeLocalTime       => x.toInstant.atZone(zoneId).toLocalTime
      case ClassOfJavaTimeZonedDateTime   => x.toInstant.atZone(zoneId)
      case ClassOfJavaUtilDate            => java.util.Date.from(x.toInstant)
      case ClassOfString                  => x.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.time.ZonedDateTime   => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x.toInstant.atZone(zoneId).toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time     .valueOf(x.toInstant.atZone(zoneId).toLocalTime)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x.toInstant.atZone(zoneId).toLocalDateTime)
      case ClassOfJavaTimeInstant         => x.toInstant
      case ClassOfJavaTimeLocalDate       => x.toInstant.atZone(zoneId).toLocalDate
      case ClassOfJavaTimeLocalDateTime   => x.toInstant.atZone(zoneId).toLocalDateTime
      case ClassOfJavaTimeLocalTime       => x.toInstant.atZone(zoneId).toLocalTime
      case ClassOfJavaTimeOffsetDateTime  => x.toOffsetDateTime
      case ClassOfJavaUtilDate            => java.util.Date.from(x.toInstant)
      case ClassOfString                  => x.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case x: java.util.Date            => targetClass match {
      case ClassOfJavaSqlDate             => java.sql.Date     .valueOf(x.toInstant.atZone(zoneId).toLocalDate)
      case ClassOfJavaSqlTime             => java.sql.Time     .valueOf(x.toInstant.atZone(zoneId).toLocalTime)
      case ClassOfJavaSqlTimestamp        => java.sql.Timestamp.valueOf(x.toInstant.atZone(zoneId).toLocalDateTime)
      case ClassOfJavaTimeInstant         => x.toInstant
      case ClassOfJavaTimeLocalDate       => x.toInstant.atZone(zoneId).toLocalDate
      case ClassOfJavaTimeLocalDateTime   => x.toInstant.atZone(zoneId).toLocalDateTime
      case ClassOfJavaTimeLocalTime       => x.toInstant.atZone(zoneId).toLocalTime
      case ClassOfJavaTimeOffsetDateTime  => x.toInstant.atZone(zoneId).toOffsetDateTime
      case ClassOfJavaTimeZonedDateTime   => x.toInstant.atZone(zoneId)
      case ClassOfString                  => new java.sql.Timestamp(x.getTime).toString
      case _                              => throwUnsupportedConversion(value, targetClass)
    }
    case _ => throwUnsupportedConversion(value, targetClass)
  }

  def convertToString(value: Any): String = value match {
    case null => null
    case _    => convertToType(value, ClassOfString).toString
  }
}

trait ValueTransformer extends ValueConverter { this: QuereaseMetadata =>
  import ValueConverter.{supportedClassNameToClass, ClassOfString}
  def convertToType(value: Any, type_ : Type): Any = {
    convertToType(value, typeNameToClass(type_.name))
  }

  private def parseColumnValue(value: Any, label: String): Any = {
    def toScala(v: Any): Any = v match {
      case m: java.util.Map[_, _]    => m.asScala.map { case (k, v) => (k, toScala(v)) }.toMap
      case a: java.util.ArrayList[_] => a.asScala.map(toScala).toList
      case x => x
    }
    val loaderSettings = LoadSettings.builder()
      .setLabel(label)
      .build()
    toScala((new Load(loaderSettings)).loadFromString(s"$value"))
  }

  protected lazy val typeNameToScalaTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("scala").orNull)
      .filter(_._2 != null)
      .toMap

  protected lazy val typeNameToClass: Map[String, Class[_]] =
    typeNameToScalaTypeName
      .map { case (tn, stn) => tn -> ValueConverter.supportedClassNameToClass.getOrElse(stn, null) }
      .filter(_._2 != null)
      .toMap

  protected def typedValue(row: RowLike, index: Int, type_ : Type): Any = {
    val stn = typeNameToScalaTypeName.get(type_.name).orNull
    row(index) match {
      case arrayString: String if arrayString startsWith "[" =>
        if  (stn == "java.lang.String")
             arrayString
        else unwrapSeq(parseColumnValue(arrayString, s"columns[$index]").asInstanceOf[Seq[_]])
      case _ =>
        row.typed(index, stn)
    }
  }

  /** Override this method to add support for collections parsed from other types (string, json, xml, ...) of values from db */
  protected def typedSeqOfValues(row: RowLike, index: Int, type_ : Type): Seq[Any] =
    row(index) match {
      case arrayString: String if arrayString startsWith "[" =>
        parseColumnValue(arrayString, s"columns[$index]").asInstanceOf[Seq[_]]
      case _ => List(typedValue(row, index, type_))
    }

  private def toCompatibleMap(row: RowLike, index: Int, view: ViewDef): Map[String, Any] = {
    val nameOrIndex =
      row.column(index).name match {
        case null => s"$index"
        case name => name
      }
    toCompatibleMap(row(index), view, nameOrIndex)
  }
  /** Supports json and yaml - override to add support for other types (xml, ...) of values from db */
  def toCompatibleMap(value: Any, view: ViewDef, nameOrIndex: String): Map[String, Any] = {
    if (value == null || value == "[]")
      null
    else parseColumnValue(value, s"${view.name}[$nameOrIndex]") match {
      case null => null
      case m: Map[String @unchecked, _] => toCompatibleMap(m, view)
      case q: Seq[_] => unwrapSeq(q) match {
        case null => null
        case m: Map[String @unchecked, _] => toCompatibleMap(m, view)
      }
    }
  }

  private def toCompatibleSeqOfMaps(row: RowLike, index: Int, view: ViewDef): Seq[Map[String, Any]] = {
    val nameOrIndex =
      row.column(index).name match {
        case null => s"$index"
        case name => name
      }
    toCompatibleSeqOfMaps(row(index), view, nameOrIndex)
  }
  /** Supports json and yaml - override to add support for collections parsed from other types (xml, ...) of values from db */
  def toCompatibleSeqOfMaps(value: Any, view: ViewDef, nameOrIndex: String): Seq[Map[String, Any]] = {
    if (value == null)
      null
    else parseColumnValue(value, s"${view.name}[$nameOrIndex]") match {
      case m: Map[String @unchecked, _] => List(toCompatibleMap(m, view))
      case q: Seq[_] => q.map {
        case m: Map[String @unchecked, _] => toCompatibleMap(m, view)
      }
    }
  }

  protected def mapSqlArray[T](arr: java.sql.Array, f: (RowLike, Int) => T): Iterator[T] = {
    import org.tresql._
    import org.tresql.given
    val tresqlResult: Result[RowLike] = arr.getResultSet
    val iterator = tresqlResult.map(f(_, 1))
    arr.free()
    iterator
  }

  protected def unwrapSeq(seq: Seq[Any]): Any =
    if (seq.lengthCompare(0) == 0)
      null
    else if (seq.lengthCompare(1) == 0)
      seq.head
    else
      sys.error(s"Expected at most one row or element, got more - can not unwrap")

  def toCompatibleSeqOfMaps(result: Result[RowLike], view: ViewDef): Seq[Map[String, Any]] =
    result.map(toCompatibleMap(_, view)).toList

  def toCompatibleMap(row: RowLike, view: ViewDef): Map[String, Any] = {
    var compatibleMap = viewNameToMapZero(view.name)
    def typed(name: String, index: Int) = view.fieldOpt(name).map { field =>
      try {
        if (field.type_.isComplexType) {
          val childView = viewDef(field.type_.name)
          if (row.column(index).isResult) {
            val childResult = row.result(index)
            val childSeq    = toCompatibleSeqOfMaps(childResult, childView)
            if (field.isCollection)
                 childSeq
            else unwrapSeq(childSeq)
          } else row(index) match {
            case arr: java.sql.Array =>
              val childSeq = mapSqlArray(arr, toCompatibleMap(_, _, childView)).toList
              if  (field.isCollection)
                   childSeq
              else unwrapSeq(childSeq)
            case x =>
              if  (field.isCollection)
                   toCompatibleSeqOfMaps(row, index, childView)
              else toCompatibleMap(row, index, childView)
          }
        } else { // simple type
          val childType = field.type_
          if (row.column(index).isResult) {
            val childResult = row.result(index)
            val childSeq    = childResult.map(typedValue(_, 0, childType)).toList
            if  (field.isCollection)
                 childSeq
            else unwrapSeq(childSeq)
          } else row(index) match {
            case arr: java.sql.Array =>
              val childSeq = mapSqlArray(arr, typedValue(_, _, childType)).toList
              if  (field.isCollection)
                   childSeq
              else unwrapSeq(childSeq)
            case x =>
              if  (field.isCollection)
                   typedSeqOfValues(row, index, childType)
              else typedValue(row, index, childType)
          }
        }
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Incompatible result for field ${view.name}.${field.fieldName}", ex)
      }
    }
    for (i <- 0 until row.columnCount) row.column(i) match {
      case c if c.name != null =>
        typed(c.name, i).foreach(value => compatibleMap += (c.name -> value))
      case _ =>
    }
    compatibleMap
  }

  def toCompatibleMap(map: Map[String, Any], view: ViewDef): Map[String, Any] = {
    var compatibleMap = viewNameToMapZero(view.name)
    def typed(name: String, value: Any) = view.fieldOpt(name).map { field =>
      try {
        if (field.type_.isComplexType) {
          val childView = viewDef(field.type_.name)
          value match {
            case null => null
            case m: Map[String @unchecked, _] =>
              val cm = toCompatibleMap(m, childView)
              if (field.isCollection) List(cm) else cm
            case q: Seq[_] =>
              val childSeq = q.map {
                case m: Map[String @unchecked, _] =>
                  toCompatibleMap(m, childView)
              }
              if  (field.isCollection)
                   childSeq
              else unwrapSeq(childSeq)
          }
        } else value match { // simple type
          case m: Map[String @unchecked, _] =>
            if (field.isCollection) List(m) else m
          case q: Seq[_] =>
            if (field.isCollection) q else unwrapSeq(q)
          case x =>
            if (field.isCollection) List(x) else x
        }
      } catch {
        case util.control.NonFatal(ex) =>
          throw new RuntimeException(s"Incompatible map for field ${view.name}.${field.fieldName}", ex)
      }
    }
    map.foreach { case (k, v) =>
      typed(k, v).foreach(value => compatibleMap += (k -> value))
    }
    compatibleMap
  }

  private def mapToJavaMap(map: Map[String, _]): java.util.Map[String, _] = {
    val result = map.map { (entry: (String, _)) =>
      (entry._1,
        entry._2 match {
          case l: Seq[_] => seqToJavaList(l)
          case m: Map[String @unchecked, _] => mapToJavaMap(m)
          case r => r
        }
      )
    }
    result.asInstanceOf[Map[String, _]].asJava
  }

  private def seqToJavaList(seq: Seq[_]): java.util.List[_] = {
    val result = seq.toList.map {
      case l: Seq[_] => seqToJavaList(l)
      case m: Map[String @unchecked, _] => mapToJavaMap(m)
      case r => r
    }
    result.asJava
  }

  private val jsonDumpSettings =
    DumpSettings.builder()
      .setDefaultFlowStyle(FlowStyle.FLOW)
      .setDefaultScalarStyle(ScalarStyle.JSON_SCALAR_STYLE)
      .setWidth(Integer.MAX_VALUE)
      .build()

  private val yamlDumpSettings =
    DumpSettings.builder()
      .setMultiLineFlow(true)
      .setDefaultFlowStyle(FlowStyle.BLOCK)
      .build()

  /** Formats values, represented as strings in json (dates, times, datetimes), passes all other values */
  protected def jsonStringValue(value: Any): Any = value match {
    case _: java.time.temporal.Temporal => convertToType(value, ClassOfString)
    case _: java.util.Date              => convertToType(value, ClassOfString)
    case _ => value
  }

  /** Converts value to be compatible with your jdbc driver, according to type.
    * Default implementation converts Map and Seq to yaml string or json string */
  def toSaveableValue(value: Any, type_ : Type): Any = {
    def javaValue = value match {
      case seq: Seq[_]                    => seqToJavaList(seq)
      case map: Map[String @unchecked, _] => mapToJavaMap(map)
    }
    def dump(dumpSettings: DumpSettings) =
      new Dump(dumpSettings).dumpToString(javaValue)
    def dumpJson(value: Any, sb: StringBuilder): Unit = value match {
      case m: Map[String @unchecked, _] =>
        sb.append('{')
        printSeq(m, sb.append(',')) { m =>
          printString(m._1, sb)
          sb.append(':')
          dumpJson(m._2, sb)
        }
        sb.append('}')
      case seq: Seq[_] =>
        sb.append('[')
        printSeq(seq, sb.append(','))(dumpJson(_, sb))
        sb.append(']')
      case s: String => printString(s, sb)
      case x => jsonStringValue(x) match {
        case s: String => sb.append(s""""$s"""")
        case x => sb.append(s"$x")
      }
    }
    // copied from spray.json.JsonPrinter to avoid dependency
    def printString(s: String, sb: StringBuilder): Unit = {
      @tailrec def firstToBeEncoded(ix: Int = 0): Int =
        if (ix == s.length) -1 else if (requiresEncoding(s.charAt(ix))) ix else firstToBeEncoded(ix + 1)
      sb.append('"')
      firstToBeEncoded() match {
        case -1 => sb.append(s)
        case first =>
          sb.append(s, 0, first)
          @tailrec def append(ix: Int): Unit =
            if (ix < s.length) {
              s.charAt(ix) match {
                case c if !requiresEncoding(c) => sb.append(c)
                case  '"' => sb.append("\\\"")
                case '\\' => sb.append("\\\\")
                case '\b' => sb.append("\\b")
                case '\f' => sb.append("\\f")
                case '\n' => sb.append("\\n")
                case '\r' => sb.append("\\r")
                case '\t' => sb.append("\\t")
                case x if x <= 0xF => sb.append("\\u000").append(Integer.toHexString(x))
                case x if x <= 0xFF => sb.append("\\u00").append(Integer.toHexString(x))
                case x if x <= 0xFFF => sb.append("\\u0").append(Integer.toHexString(x))
                case x => sb.append("\\u").append(Integer.toHexString(x))
              }
              append(ix + 1)
            }
          append(first)
      }
      sb.append('"')
    }
    def printSeq[A](iterable: Iterable[A], printSeparator: => Unit)(f: A => Unit): Unit = {
      var first = true
      iterable.foreach { a =>
        if (first) first = false else printSeparator
        f(a)
      }
    }
    def requiresEncoding(c: Char): Boolean =
      c match {
        case '"'  => true
        case '\\' => true
        case c    => c < 0x20
      }

    def dumpToString =
      type_.name match {
        case "yaml" => dump(yamlDumpSettings)
        case  json  =>
          val sb = new StringBuilder
          dumpJson(value, sb)
          sb.toString
      }
    value match {
      case _: Map[String @unchecked, _] =>
        dumpToString
      case _: Seq[_] =>
        dumpToString
      case x => x
    }
  }

  /** Provides missing fields, uses [[toSaveableValue]] to convert values */
  def toSaveableMap(map: Map[String, scala.Any], view: ViewDef): Map[String, Any] = {
    (if  (view.fields.forall(f => map.contains(f.fieldName) || isOptionalField(f)))
          map
     else viewNameToMapZero(view.name) ++ map
    ) ++
      view.fields
        .filter(f => map.contains(f.fieldName) || !isOptionalField(f))
        .map(f => f.fieldName -> (try {
          lazy val valueType = tableMetadata.columnDefOption(view, f).map(_.type_).getOrElse(f.type_)
          lazy val childView = viewDef(f.type_.name)
          lazy val shouldSaveAsValue = !f.type_.isComplexType ||
            childView.table == null && (childView.joins == null || childView.joins == Nil)
          def toSaveableMapOrValue(value: Any) = value match {
            case null => null
            case m: Map[String @unchecked, _] =>
              if (shouldSaveAsValue) {
                if (f.type_.isComplexType)
                  toSaveableValue(toCompatibleMap(m, childView), valueType)
                else
                  toSaveableValue(m, valueType)
              } else
                toSaveableMap(m, childView)
            case x =>
              if (shouldSaveAsValue) {
                if (f.type_.isComplexType)
                  toSaveableValue(toCompatibleMap(x, childView, f.fieldName), valueType)
                else
                  toSaveableValue(x, valueType)
              } else
                toSaveableMap(toCompatibleMap(x, childView, f.fieldName), childView)
          }
          map.getOrElse(f.fieldName, null) match {
            case null if f.isCollection => Nil
            case null => null
            case Nil  if f.isCollection => Nil
            case Nil  if f.type_.name == "json" || f.type_.name == "yaml" => "[]"
            case Nil  => null
            case m: Map[String @unchecked, _] =>
              if (f.isCollection)
                List(toSaveableMapOrValue(m))
              else
                toSaveableMapOrValue(m)
            case seq: Seq[_] =>
              if (f.isCollection)
                seq.map(toSaveableMapOrValue)
              else if (shouldSaveAsValue && !f.type_.isComplexType)
                toSaveableValue(seq, valueType)
              else
                toSaveableMapOrValue(unwrapSeq(seq))
            case x =>
              if (shouldSaveAsValue)
                toSaveableMapOrValue(x)
              else if (f.isCollection)
                toCompatibleSeqOfMaps(x, childView, f.fieldName)
                  .map(toSaveableMap(_, childView))
              else
                toSaveableMap(toCompatibleMap(x, childView, f.fieldName), childView)
          }
        } catch {
          case util.control.NonFatal(ex) =>
            throw new RuntimeException(s"Failed to convert field ${view.name}.${f.fieldName} to saveable map or value", ex)
        }))
  }
}

class Querease extends QueryStringBuilder with ValueTransformer
  with FilterTransformer with QuereaseExpressions with QuereaseMetadata with QuereaseResolvers {

  private def ortDbPrefix(db: String): String       = Option(db).map(db => s"@$db:") getOrElse ""
  private def ortAliasSuffix(alias: String): String = Option(alias).map(" " + _) getOrElse ""
  private def tablesToSaveTo(viewDef: ViewDef) =
    if (viewDef.saveTo == null || viewDef.saveTo.isEmpty)
      Option(viewDef.table).filter(_ != "")
        .map(t => Seq(ortDbPrefix(viewDef.db) + t + ortAliasSuffix(viewDef.tableAlias)))
        .getOrElse(throw new RuntimeException(s"Unable to save - target table name for view ${viewDef.name} is not known"))
    else viewDef.saveTo.map(t => if (t startsWith "@") t else ortDbPrefix(viewDef.db) + t)

  protected def idToLong(id: Any): Long = id match {
    case id: Long   => id
    case id: Int    => id
    case id: Short  => id
    case xx         => 0L
  }

  // extraPropsToSave allows to specify additional columns to be saved that are not present in pojo.
  def save[B <: AnyRef](
    pojo: B,
    extraPropsToSave: Map[String, Any] = null,
    forceInsert: Boolean = false,
    filter: String = null,
    params: Map[String, Any] = null)(implicit resources: Resources, qio: QuereaseIo[B]): Long = {
    val pojoPropMap = qio.toMap(pojo)
    val view = viewDefFromMf(ManifestFactory.classType(pojo.getClass))
    val method = if (forceInsert) Insert else Save
    save(view, pojoPropMap, extraPropsToSave, method, filter, params)
  }

  def save[B <: AnyRef](
    view:   ViewDef,
    data:   Map[String, Any],
    extraPropsToSave: Map[String, Any],
    method: SaveMethod,
    filter: String,
    params: Map[String, Any],
  )(implicit resources: Resources, qio: QuereaseIo[B]): Long = {
    validate(view, data, Option(params).getOrElse(Map()))
    val propMap = data ++ (if (extraPropsToSave != null) extraPropsToSave
      else Map()) ++ Option(params).getOrElse(Map())
    val keyFields = viewNameToKeyFields(view.name)
    lazy val id = viewNameToIdFieldName.get(view.name).flatMap(propMap.get).filter(_ != null)
    val resolvedMethod =
      if  (method == Save)
        if (keyFields.forall(f => propMap.get(f.fieldName).orNull == null)) Insert else Update
      else method
    resolvedMethod match {
      case Insert =>
        insert(view, propMap, filter, extraPropsToSave)
      case Update =>
        update(view, propMap, filter, extraPropsToSave)
        Try(id.get.toString.toLong) getOrElse 0L
      case Upsert =>
        upsert(view, propMap, filter, extraPropsToSave) match {
          case (Insert, insertedId) =>
            insertedId
          case (Update, _)          =>
            Try(id.get.toString.toLong) getOrElse 0L
        }
    }
  }

  private def mergeFilters(filterOpt: Option[String], extraFilter: String): Option[String] = {
    if   (extraFilter == null)  filterOpt
    else if (filterOpt.isEmpty) Option(extraFilter)
    else filterOpt.map { f => Seq(f, extraFilter).map(a => s"($a)").mkString(" & ") }
  }

  private def addExtraPropsToMetadata(metadata: OrtMetadata.View, extraPropsToSave: Map[String, Any]): OrtMetadata.View = {
    if (extraPropsToSave == null || extraPropsToSave.isEmpty) {
      metadata
    } else {
      val mdCols  = metadata.properties.map(_.col).toSet
      val xpCols  = extraPropsToSave.keySet
      val missing = xpCols -- mdCols
      if (missing.isEmpty) {
        metadata
      } else {
        metadata.copy(
          properties = metadata.properties ++ missing.map { col =>
            OrtMetadata.Property(
              col   = col,
              value = OrtMetadata.TresqlValue(
                tresql    = s":$col",
              ),
              optional  = false,
              forInsert = true,
              forUpdate = true,
            )
          }
        )
      }
    }
  }

  protected def insert(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val metadata = persistenceMetadata(view, data) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(insert = mergeFilters(f.insert, filter))
          },
        )
      }
    insert(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def insert(
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val metadata = persistenceMetadata(view, data)
    insert(view, metadata, data)
  }

  protected def insert(
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources): Long = {
    val result = ORT.insert(metadata, toSaveableMap(data, view))
    val (insertedRowCount, id) =
      (result.count.get, result.id.orNull)
    if (insertedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not inserted into table(s): ${tables.mkString(",")}")
    }
    else idToLong(id)
  }

  protected def update[B <: AnyRef](
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources, qio: QuereaseIo[B]): Unit = {
    val metadata = persistenceMetadata(view, data) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(update = mergeFilters(f.update, filter))
          },
        )
      }
    update(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def update[B <: AnyRef](
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources, qio: QuereaseIo[B]): Unit = {
    val metadata = persistenceMetadata(view, data)
    update(view, metadata, data)
  }

  protected def update[B <: AnyRef](
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources, qio: QuereaseIo[B]): Unit = {
    val updatedRowCount = ORT.update(metadata, toSaveableMap(data, view)).count getOrElse -1
    if (updatedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not updated in table(s): ${tables.mkString(",")}")
    }
  }

  protected def upsert(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    extraPropsToSave: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val metadata = persistenceMetadata(view, data) match {
        case md if filter == null => md
        case md => md.copy(
          filters = md.filters.orElse(Some(OrtMetadata.Filters())).map { f =>
            f.copy(
              insert = mergeFilters(f.insert, filter),
              update = mergeFilters(f.update, filter),
            )
          },
        )
      }
    upsert(view, addExtraPropsToMetadata(metadata, extraPropsToSave), data)
  }

  def upsert(
    view: ViewDef,
    data: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val metadata = persistenceMetadata(view, data)
    upsert(view, metadata, data)
  }

  protected def upsert(
    view: ViewDef,
    metadata: OrtMetadata.View,
    data: Map[String, Any],
  )(implicit resources: Resources): (SaveMethod, Long) = {
    val result = ORT.save(metadata, toSaveableMap(data, view))
    val (method, affectedRowCount, id) = result match {
      case ir: InsertResult => (Insert, result.count.get, result.id.orNull)
      case ur: UpdateResult => (Update, result.count.get, 0L)
    }
    if (affectedRowCount == 0) {
      val tables = metadata.saveTo.map(_.table)
      throw new NotFoundException(
        s"Record not upserted in table(s): ${tables.mkString(",")}")
    } else (method, idToLong(id))
  }
  def validationResults[B <: AnyRef](pojo: B, params: Map[String, Any]
    )(implicit resources: Resources, qio: QuereaseIo[B]): List[ValidationResult] = {
    val view = viewDefFromMf(ManifestFactory.classType(pojo.getClass))
    validationResults(view, qio.toMap(pojo), params)
  }
  def validationResults(view: ViewDef, data: Map[String, Any], params: Map[String, Any])(
    implicit resources: Resources): List[ValidationResult] = {
    def validateView(viewDef: ViewDef, obj: Map[String, Any]): List[String] =
      validationsQueryString(viewDef) match {
        case Some(query) =>
          Query(query, obj).map(_.s("msg")).toList
        case _ => Nil
      }
    def validateViewAndSubviews(path: List[Any], viewDef: ViewDef, obj: Map[String, Any],
                                res: List[ValidationResult]): List[ValidationResult] = {
      val viewRes =
        validateView(viewDef, obj ++ params) match {
          case messages if messages.nonEmpty => List(ValidationResult(path.reverse, messages))
          case _ => Nil
        }
      viewDef.fields
        .collect { case f if f.type_.isComplexType => (f.name, nameToViewDef(f.type_.name)) }
        .foldLeft(viewRes ::: res) { (r, nv) =>
          val (n, vd) = nv
          def maybeAddParent(m: Map[String, Any]) =
            if(!m.contains("__parent")) m + ("__parent" -> obj) else m
          obj.get(n).map {
            case m: Map[String @unchecked, _] =>
              validateViewAndSubviews(n :: path, vd, maybeAddParent(m), r)
            case l: List[Map[String, _]@unchecked] =>
              val p = n :: path
              l.zipWithIndex.foldLeft(r){ (r1, owi) =>
                val (o, i) = owi
                validateViewAndSubviews(i :: p, vd, maybeAddParent(o), r1)
              }
            case _ => r
          }.getOrElse(r)
        }
    }
    validateViewAndSubviews(Nil, view, data, Nil).reverse
  }
  def validate[B <: AnyRef](pojo: B, params: Map[String, Any])(implicit resources: Resources, qio: QuereaseIo[B]): Unit = {
    val view = viewDefFromMf(ManifestFactory.classType(pojo.getClass))
    validate(view, qio.toMap(pojo), params)
  }
  def validate(view: ViewDef, data: Map[String, Any], params: Map[String, Any])(implicit resources: Resources): Unit = {
    val results = validationResults(view, data, params)
    results.flatMap(_.messages).filterNot(_ == null).filterNot(_ == "") match {
      case messages if messages.nonEmpty => throw new ValidationException(messages.mkString("\n"), results)
      case _ => ()
    }
  }

  def countAll[B <: AnyRef: Manifest](params: Map[String, Any],
    extraFilter: String = null, extraParams: Map[String, Any] = Map())(
      implicit resources: Resources, qio: QuereaseIo[B]) = {
      countAll_(viewDefFromMf[B], params, extraFilter, extraParams)
  }
  protected def countAll_(viewDef: ViewDef, params: Map[String, Any],
    extraFilter: String = null, extraParams: Map[String, Any] = Map())(
      implicit resources: Resources): Int = {
    val (tresqlQueryString, paramsMap) =
      queryStringAndParams(viewDef, params, 0, 0, "", extraFilter, extraParams, null, true)
    Query(tresqlQueryString, paramsMap).unique.int(0)
  }

  def list[B <: AnyRef: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilter: String = null, extraParams: Map[String, Any] = Map(),
      fieldFilter: FieldFilter = null)(
        implicit resources: Resources, qio: QuereaseIo[B]): List[B] = {
    result(params, offset, limit, orderBy, extraFilter, extraParams, fieldFilter).toList
  }
  def result[B <: AnyRef: Manifest](params: Map[String, Any],
      offset: Int = 0, limit: Int = 0, orderBy: String = null,
      extraFilter: String = null, extraParams: Map[String, Any] = Map(),
      fieldFilter: FieldFilter = null)(
        implicit resources: Resources, qio: QuereaseIo[B]): QuereaseIteratorResult[B] = {
    result(rowsResult(viewDefFromMf[B], params, offset, limit, orderBy, extraFilter, extraParams, fieldFilter))
  }
  def rowsResult(viewDef: ViewDef, params: Map[String, Any],
      offset: Int, limit: Int, orderBy: String,
      extraFilter: String, extraParams: Map[String, Any],
      fieldFilter: FieldFilter)(
        implicit resources: Resources): Result[RowLike] = {
    val (q, p) = queryStringAndParams(viewDef, params,
        offset, limit, orderBy, extraFilter, extraParams, fieldFilter)
    Query(q, p)
  }

  def get[B <: AnyRef](id: Long)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), null, null, null)
  def get[B <: AnyRef](id: Long, fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), null, null, fieldFilter)
  def get[B <: AnyRef](id: Long, extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), extraFilter, null, null)
  def get[B <: AnyRef](id: Long, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), null, extraParams, null)
  def get[B <: AnyRef](id: Long, extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), extraFilter, extraParams, null)
  def get[B <: AnyRef](id: Long, extraFilter: String, extraParams: Map[String, Any], fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(id), extraFilter, extraParams, fieldFilter)

  def get[B <: AnyRef](code: String)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), null, null, null)
  def get[B <: AnyRef](code: String, fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), null, null, fieldFilter)
  def get[B <: AnyRef](code: String, extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), extraFilter, null, null)
  def get[B <: AnyRef](code: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), null, extraParams, null)
  def get[B <: AnyRef](code: String, extraFilter: String, extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), extraFilter, extraParams, null)
  def get[B <: AnyRef](code: String, extraFilter: String, extraParams: Map[String, Any], fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(Seq(code), extraFilter, extraParams, fieldFilter)

  def get[B <: AnyRef](keyValues: Seq[Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(keyValues, null, null, null)
  def get[B <: AnyRef](keyValues: Seq[Any], fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(keyValues, null, null, fieldFilter)
  def get[B <: AnyRef](keyValues: Seq[Any], extraFilter: String)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(keyValues, extraFilter, null, null)
  def get[B <: AnyRef](keyValues: Seq[Any], extraParams: Map[String, Any])(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = get(keyValues, null, extraParams, null)

  // TODO if view contains fields for columns - use field names, otherwise use column names?
  def get[B <: AnyRef](keyValues: Seq[Any],
      extraFilter: String, extraParams: Map[String, Any], fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = {
    val view = viewDefFromMf[B]
    keyValues match {
      case Nil =>
        get(keyValues, Nil, extraFilter, extraParams, fieldFilter)
      case Seq(id: Long) =>
        val keyColName = viewNameToKeyColNameForGetById(view.name)
        if (keyColName == null)
          sys.error(s"Key of long type not found for $mf, can not get")
        get(keyValues, Seq(keyColName), extraFilter, extraParams, fieldFilter)
      case Seq(code: String) =>
        val keyColName = viewNameToKeyColNameForGetByCode(view.name)
        if (keyColName == null)
          sys.error(s"Key of string type not found for $mf, can not get")
        get(keyValues, Seq(keyColName), extraFilter, extraParams, fieldFilter)
      case values =>
        val keyColNames = viewNameToKeyColNames(view.name)
        if (keyColNames.isEmpty)
          sys.error(s"Key not found for $mf, can not get")
        get(keyValues, keyColNames, extraFilter, extraParams, fieldFilter)
    }
  }
  def get[B <: AnyRef](keyValues: Seq[Any], keyColNames: Seq[String],
      extraFilter: String, extraParams: Map[String, Any], fieldFilter: FieldFilter)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): Option[B] = {
    get(viewDefFromMf[B], keyValues, keyColNames, extraFilter, extraParams, fieldFilter).map { row =>
      val converted = qio.convertRow[B](row)
      row.close()
      converted
    }
  }

  def get(
    viewDef:     ViewDef,
    keyValues:   Seq[Any],
    keyColNames: Seq[String],
    extraFilter: String,
    extraParams: Map[String, Any],
    fieldFilter: FieldFilter,
  )(implicit resources: Resources): Option[RowLike] = {
    import viewDef.name
    val qualifier = baseFieldsQualifier(viewDef)
    val prefix = Option(qualifier).map(_ + ".") getOrElse ""
    if (keyColNames.length != keyValues.length)
      sys.error(
        s"Expecting ${keyColNames.length} value(s) as key for $name but got ${keyValues.length}, can not get")
    val keysAndValues = keyColNames zip keyValues
    val keyFilter = keysAndValues.map {
      case (col, value) => s"${prefix}${col} = ${if (value == null) "null" else s":${col}"}"
    }.mkString(" & ")
    val params = keysAndValues.toMap
    val extraQ = extraFilter match {
      case null | "" =>    keyFilter
      case x => s"[$x] & [$keyFilter]"
    }
    val extraP = extraParams match {
      case null => Map[String, Any]()
      case m => m
    }
    val (q, p) = queryStringAndParams(viewDef,
      params, 0, 2, "", extraQ, extraP, fieldFilter)
    try Query(q, p).uniqueOption catch {
      case ex: org.tresql.MissingBindVariableException  => throw ex
      case ex: org.tresql.TresqlException               => throw ex
      case util.control.NonFatal(ex) =>
        throw new RuntimeException(
          s"Failed to get unique optional row for view $name: ${ex.getMessage}", ex)
    }
  }

  def create(
    view:   ViewDef,
    params: Map[String, Any],
  )(implicit resources: Resources): RowLike = {
    import QuereaseMetadata.AugmentedQuereaseFieldDef
    import view.name
    val rootDbPrefix  = Option(view.db).map(db => s"|$db:").getOrElse("")
    val childDbPrefix = Option(view.db).map(db =>  s"$db:").getOrElse("")
    val q =
        view.fields.map { f =>
          val expr =
            if (f.initial != null)
              f.initial
            else if (view.table != null && f.type_ != null && f.type_.isComplexType)
              s"|$childDbPrefix[false]${view.table}[false]{0}" // XXX FIXME providing child result - expected by current QuereaseIo implementation
            else
              "null"
          expr + " " + f.fieldName
        }.mkString(s"$rootDbPrefix{", ", ", "}")
    try Query(q, params).uniqueOption.get catch {
      case ex: org.tresql.MissingBindVariableException  => throw ex
      case ex: org.tresql.TresqlException               => throw ex
      case util.control.NonFatal(ex) =>
        throw new RuntimeException(
          s"Failed to execute query to create data for new instance, view $name: ${ex.getMessage}", ex)
    }
  }

  def create[B <: AnyRef](params: Map[String, Any] = Map.empty)(
      implicit mf: Manifest[B], resources: Resources, qio: QuereaseIo[B]): B = {
    import QuereaseMetadata.AugmentedQuereaseFieldDef
    val view = this.viewDefFromMf
    if (view.fields.exists(_.initial != null)) {
      Option(create(view, params)).map { row =>
        val converted = qio.convertRow[B](row)
        row.close()
        converted
      }.get
    } else {
      mf.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[B]
    }
  }

  def list[B <: AnyRef: Manifest](query: String, params: Map[String, Any])(
    implicit resources: Resources, qio: QuereaseIo[B]) =
    result(query, params).toList
  def result[B <: AnyRef: Manifest](query: String, params: Map[String, Any])(
      implicit resources: Resources, qio: QuereaseIo[B]): QuereaseIteratorResult[B] =
    result(Query(query, params))
  private def result[B <: AnyRef: Manifest](queryReult: Result[RowLike])(
      implicit resources: Resources, qio: QuereaseIo[B]): QuereaseIteratorResult[B] =
    new QuereaseIteratorResult[B] {
      private val result = queryReult
      override def hasNext = result.hasNext
      override def next() = qio.convertRow[B](result.next())
      override def close = result.close
      override def view: ViewDef = viewDefFromMf[B]
    }

  def delete[B <: AnyRef](instance: B, filter: String = null, params: Map[String, Any] = null)(
    implicit resources: Resources, qio: QuereaseIo[B]): Int = {
    val view = viewDefFromMf(ManifestFactory.classType(instance.getClass))
    val propMap = qio.toMap(instance)
    delete(view, propMap, filter, params)
  }

  def delete(
    view:   ViewDef,
    data:   Map[String, Any],
    filter: String,
    params: Map[String, Any],
  )(implicit resources: Resources): Int = {
    val mergedFilter = persistenceMetadata(view, data) match {
        case md if filter == null => md.filters.flatMap(_.delete).orNull
        case md => mergeFilters(md.filters.flatMap(_.delete), filter).orNull
      }
    val key_fields = viewNameToKeyFields(view.name)
    if (key_fields.isEmpty)
      sys.error(s"Key not found for view ${view.name}, can not delete")
    val key        = key_fields.map(f => f.name)
    val keyPropMap = key_fields.map(f => f.name -> data.getOrElse(f.fieldName, null)).toMap
    val delCount   = ORT.delete(
      ortDbPrefix(view.db) + view.table + ortAliasSuffix(view.tableAlias),
      key,
      if (params == null || params.isEmpty) keyPropMap else keyPropMap ++ params,
      mergedFilter,
    ).count.get
    if (delCount == 0)
      throw new NotFoundException(s"Record not deleted in table ${view.table}")
    else delCount
  }
}

object QueryStringBuilder {
  case class CompilationUnit(
    category: String, // queries | validations
    source:   String, // view name
    db:       String, // context database (or alias) for query
    query:    String, // query string to be compiled
  ) {
    val queryStringWithContext = s"[${category}${if (db == null) "" else s" @ $db"}]: $query"
  }
}

trait QueryStringBuilder {
  this: QuereaseMetadata with QuereaseExpressions with FilterTransformer with QuereaseResolvers =>

  /*
  val ComparisonOps = "= < > <= >= != ~ ~~ !~ !~~".split("\\s+").toSet
  def comparison(comp: String) =
    if (ComparisonOps.contains(comp)) comp
    else sys.error("Comparison operator not supported: " + comp)
  // TODO languagePrefs, i18nExpr? overridable?
  def languagePreferences: List[String] = List("lv", "en", "ru")
  def getI18nColumnExpression(qName: String) = {
    val langs = languagePreferences
    val lSuff = langs.map {
      case "lv" => ""
      case "en" => "_eng"
      case "ru" => "_rus"
    }
    lSuff.tail.foldLeft(qName + lSuff(0))((expr, suff) => "nvl(" + expr + ", " + qName + suff + ")")
  }
  */
  private def regex(pattern: String) = ("^" + pattern + "$").r
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  protected val FieldRefRegexp = regex(s"\\^($ident)\\.($ident)\\s*(\\[(.*)\\])?")

  def validationsQueryString(viewDef: ViewDef): Option[String] = {
    import QuereaseMetadata.AugmentedQuereaseViewDef
    validationsQueryString(viewDef, viewDef.validations)
  }

  def validationsQueryString(viewDef: ViewDef,
                             validations: Seq[String]): Option[String] = {
    val result =
      if (validations != null && validations.nonEmpty) {
        def tresql(cursorsView: String, vs: Seq[String]): String = {
          def error(exp: Exp) = {
            val msg =
              s"Validation expression must consist of" +
                s" two or three comma separated expressions. One before last is boolean expression" +
                s" which evaluated to false returns last string expression as error message." +
                s" In the case of three expressions first of them should be cursor definitions which can" +
                s" be used later in boolean and error message expressions." +
                s" Instead got: ${exp.tresql}"
            sys.error(msg)
          }

          var cursorList = List[String]()
          val valTresql =
            "messages(# idx, msg) {" +
              vs.zipWithIndex.map {
                case (v, i) =>
                  val valExp =
                    parser.parseExp(v) match {
                      case Arr(valExps) if valExps.size == 3 =>
                        valExps.head match {
                          case With(cursors, _) =>
                            cursorList = cursors.map(_.tresql).mkString(", ") :: cursorList
                          case x => error(x)
                        }
                        valExps.tail.map(_.tresql).mkString(", ")
                      case Arr(valExps) if valExps.size == 2 => v
                      case x => error(x)
                    }
                  s"{ $i idx, if_not($valExp) msg }"
              }.mkString(" + ") +
              s"} ${if (cursorsView == null) "" else s"[build_cursors($cursorsView)]"}" +
              s"messages[msg != null] { msg } #(idx)"
          if (cursorList.isEmpty) valTresql
          else cursorList.reverse.mkString(", ") + ", " + valTresql
        }

        val validations_head = validations.head.trim
        if (validations_head.startsWith(BindVarCursorsForViewCmd)) {
          val view_name = validations_head match {
            case BindVarCursorsForViewCmdRegex(view_name) => view_name
            case _ => sys.error(s"Missing view name in '$validations_head'")
          }
          tresql(view_name, validations.tail)
        } else if (validations_head.startsWith(BindVarCursorsCmd)) {
          tresql(viewDef.name, validations.tail)
        } else tresql(null, validations)
      } else null

    Option(result)
  }
  def validationsQueryStrings(viewDef: ViewDef): Seq[String] = {
    val visited: collection.mutable.Set[String] = collection.mutable.Set.empty
    def vqsRecursively(viewDef: ViewDef): Seq[String] = {
      if (visited contains viewDef.name) Nil
      else {
        visited += viewDef.name
        validationsQueryString(viewDef).toList ++
          viewDef.fields.flatMap { f =>
            if (f.type_.isComplexType)
              vqsRecursively(this.viewDef(f.type_.name))
            else Nil
          }
      }
    }
    vqsRecursively(viewDef)
  }

  protected def unusedName(name: String, usedNames: collection.Set[String]): String = {
    @tailrec
    def unusedName(index: Int): String =
      // FIXME ensure max identifier length for db is not exceeded!
      if (!(usedNames contains (name + "_" + index))) name + "_" + index
      else unusedName(index + 1)
    if (!usedNames.contains(name)) name else unusedName(2)
  }
  def baseFieldsQualifier(view: ViewDef): String = {
    // TODO do not parse multiple times!
    def parsedJoins =
      Option(view.joins).map(joinsParser(view.db, tableAndAlias(view), _))
        .getOrElse(Nil)
    Option(view.tableAlias)
      .orElse(if (view.joins == null || view.joins == Nil) Some(view.table) else None)
      .getOrElse(parsedJoins
        .filter(_.table == view.table).toList match {
          case Join(a, _, _, _) :: Nil =>
            // qualifier, if base table encountered only once
            Option(a) getOrElse view.table
          case Nil => null // no qualifier
          case baseTableJoinsList =>
            // b - base table alias
            if (baseTableJoinsList.exists(_.alias == "b")) "b" else null
        })
  }
  def queryStringAndParams(view: ViewDef, params: Map[String, Any],
    offset: Int = 0, limit: Int = 0, orderBy: String = null,
    extraFilter: String = null, extraParams: Map[String, Any] = Map(),
    fieldFilter: FieldFilter = null,
    countAll: Boolean = false, includeDbPrefix: Boolean = true,
  ): (String, Map[String, Any]) = {
    val (from, pathToAlias) = this.fromAndPathToAlias(view)
    val where = this.where(view, extraFilter, pathToAlias)
    val groupBy = this.groupBy(view)
    val having = this.having(view)
    val simpleCountAll = countAll && groupBy == "" && having == ""
    val cols = this.cols(view, simpleCountAll, pathToAlias, fieldFilter)
    val order = this.order(view, orderBy)
    val values = Option(params) getOrElse Map[String, Any]() // TODO convert?
    val dbPrefix = if (includeDbPrefix) Option(view.db).map(db => s"|$db:").getOrElse("") else ""

    val (q1, limitOffsetPars) =
      limitOffset(dbPrefix + from + where + cols + groupBy + having + order, countAll, limit, offset)
    val q = if (countAll && !simpleCountAll) s"($q1) a {count(*)}" else q1
    // TODO param name?
    (q, values ++ extraParams ++ limitOffsetPars.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap)
  }
  def queryString(view: ViewDef, fields: Seq[FieldDef], exprFields: Seq[FieldDef], filter: String): String = {
    val (from, pathToAlias) = this.fromAndPathToAlias(view, (fields ++ exprFields).filter(_ != null))
    val where = Option(filter).filter(_ != "")
      .map(filter => if (pathToAlias.size > 1) qualify(view, filter, pathToAlias, ignoreUnknownPaths = true) else filter)
      .map("[" + _ + "]").mkString match { case "" => "" case a => a }
    val groupBy = this.groupBy(view)
    val having = this.having(view)
    val cols = "{" +
      fields.map(field =>
        queryColExpression(view, field, pathToAlias, null) +
          Option(queryColAlias(field)).map(" " + _).getOrElse("")
      ).mkString(", ") +
    "}"
    from + where + " " + cols + groupBy + having
  }

  /*
  val paramsFilter =
    Option(params).map(_.Filter).filter(_ != null).map(_.toList) getOrElse Nil
  import org.mojoz.metadata.Naming.{ dbNameToXsdName => xsdName }
  val fieldNameToDefMap = view.fields.map(f => xsdName(f.fieldName) -> f).toMap
  // FIXME extra order by, injection-safe!
  val safeExpr = List("decode(cnt, null, 0, 1)",
    "decode(sign(next_reregistration_date - sysdate), 1, 0, 0, 0, 1)")
    .map(expr => (expr,
      FieldDef("", "", "", "", false, null, true, true, expr,
        true, false, null, null, null, null, false, "")))
    .toMap
  def fieldNameToDef(f: String) = fieldNameToDefMap.getOrElse(f,
    safeExpr.get(f) getOrElse
      sys.error("Field " + f + " is not available from view " + xsdName(view.name)))
  def isFilterable(f: ListFilterType): Boolean =
    if (!fieldNameToDef(f.Field).isFilterable) sys.error("Calculated field " + f.Field +
      " is not available for filtering from view " + xsdName(view.name))
    else true
  val filteredParams = Option(params).getOrElse(
    new ListRequestType(0, 0, Array(), Array())).copy(Filter =
      paramsFilter.filter(f => !extraParams.contains(f.Field)).filter(isFilterable).toArray)
  import filteredParams.{ Sort => sort, Offset => offset }
  //LIMIT threshold
  // TODO support overridable maxLimit?
  val limit = math.min(100, filteredParams.Limit)
  //list of tuples (bind variable name -> ListFilterType)
  val filter = filteredParams.Filter.groupBy(_.Field).toList.flatMap(f =>
    if (f._2.size > 1) f._2.zipWithIndex.map(t => (f._1 + t._2) -> t._1) else
      f._2.toList.map(f._1 -> _)).toArray

  val preferRu = languagePreferences(0) == "ru"
  def isRu(f: FieldDef) = preferRu && f.isI18n
  */
  private def isI18n(f: FieldDef) = false // f.isI18n
  def queryColTableAlias(view: ViewDef, f: FieldDef): String =
    Option(f.tableAlias) getOrElse
      (if (f.table == view.table) baseFieldsQualifier(view) else f.table)

  private def getChildViewDef(viewDef: ViewDef, fieldDef: FieldDef) =
    scala.util.Try(this.viewDef(fieldDef.type_.name)).toOption.getOrElse(
      sys.error("Child viewDef not found: " + fieldDef.type_.name +
        " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))

  def qualify(view: ViewDef, expression: String,
    pathToAlias: Map[List[String], String], ignoreUnknownPaths: Boolean = false): String = {
    parser.transformTresql(expression, {
      // do not transform subqueries (skip deeper analysis)
      case q: QueryParser_Query => q
      case ident @ Ident(i) if !i.startsWith("^") =>
        if (i.lengthCompare(1) == 0)
          if (tableMetadata.col(view.table, i.head, view.db).isDefined)
            Ident((baseFieldsQualifier(view) :: i).filter(_ != null))
          else ident // no-arg function or unknown pseudo-col
        else if (ignoreUnknownPaths)
          pathToAlias.get(i dropRight 1)
            .map(a => Ident(a :: (i takeRight 1)))
            .getOrElse(ident)
        else Ident(pathToAlias(i dropRight 1) :: (i takeRight 1))
    })
  }
  protected def childDbPrefix(childViewDef: ViewDef): String = {
    lazy val hasNamedDbAncestor =
      childViewNameToAncestorDbNames.get(childViewDef.name)
        .exists(_.exists(_ != null))
    Option(childViewDef.db).orElse(Some("").filter(_ => hasNamedDbAncestor)).map(_ + ":").getOrElse("")
  }
  def queryColExpression(view: ViewDef, f: FieldDef,
      pathToAlias: Map[List[String], String],
      fieldFilter: FieldFilter,
  ): String = {
    val qName = Option(queryColTableAlias(view, f))
      .map(_ + "." + f.name) getOrElse f.name // TODO use pathToAlias!
    if (f.expression != null)
      qualify(
        view,
        transformExpression(
          f.expression, view, f, QuereaseExpressions.Field, baseFieldsQualifier(view), pathToAlias),
        pathToAlias
      )
    else if (f.type_ != null && f.type_.isComplexType) {
      val childViewDef = getChildViewDef(view, f)
      val childFieldFilter = if (fieldFilter == null) null else fieldFilter.childFilter(f.fieldName)
      def joinToParent = Option(f.joinToParent).orElse {
        if (view.table != null && childViewDef.table != null) {
          val t1 = Option(view.tableAlias).getOrElse(view.table)
          val t2 = Option(childViewDef.tableAlias).getOrElse(childViewDef.table)
          tableMetadata.tableDef(view).refs
            .find(r =>
              Option(r.defaultRefTableAlias).getOrElse(r.refTable) == f.name &&
              r.refTable == childViewDef.table)
            .map { r =>
              r.cols.zip(r.refCols).map {
                case (col, refCol) => s"$t1.$col = $t2.$refCol"
              }.mkString("[", " & ", "]")
            }
        }
        else None
      }.getOrElse("")
      def sortDetails = Option(f.orderBy match {
        case null => childViewDef.orderBy match {
          case null | Nil =>
            val prefix = baseFieldsQualifier(childViewDef) match {
              case null => ""
              case q => q + "."
            }
            // preserve detail ordering
            if (childViewDef.table != null)
              tableMetadata.tableDef(childViewDef).pk
                .map(_.cols.map(prefix + _) mkString ", ").orNull
            else null
          case ord => ord mkString ", "
        }
        case ord =>
            ord.trim
      }) /* TODO ? .map(Naming.dbNameToXsdName) */ .orNull
      /*
      lazy val sortDetailsDbName = Naming.xsdNameToDbName(sortDetails)
      val isSortFieldIncluded = sortDetails == null ||
        childViewDef.fields.map(_.fieldName).toSet
        .contains(sortDetailsDbName)
      val extendedChildViewDef = // XXX add missing TODO GET RID OF THIS BS
        if (isSortFieldIncluded) childViewDef
        else {
          val fd = FieldDef(childViewDef.table, null, sortDetailsDbName,
            null, false, null, false, true, null, true, false,
            null, null, null, null, false, null)
          childViewDef.copy(fields = childViewDef.fields ++
            Seq(fd.copy(type_ = columnDef(childViewDef, fd).type_)))
        }
      */
      if (childViewDef.table == null && (childViewDef.joins == null || childViewDef.joins == Nil))
       if (view.table == null || f.table != null)
        qName
       else
        s"|[false]${view.table}[false]{0}" // XXX FIXME providing child result - expected by current QuereaseIo implementation
      else if (view.name == childViewDef.name)
        s"(|$joinToParent)"
      else {
        val (tresqlQueryString, _) =
          queryStringAndParams(childViewDef, null, 0, 0, sortDetails,
            fieldFilter = childFieldFilter, includeDbPrefix = false)
        "|" + childDbPrefix(childViewDef) + joinToParent + tresqlQueryString
      }
    } // TODO? else if (isI18n(f)) getI18nColumnExpression(qName)
    else qName
  }

  def queryColAlias(f: FieldDef): String =
    Option(f.alias) getOrElse {
      if (f.isExpression && f.expression != null || isI18n(f)) f.name
      else if (f.type_ != null && f.type_.isComplexType && f.isCollection) f.name // FIXME toPlural(f.name)
      else if (f.type_ != null && f.type_.isComplexType) f.name
      else null
    }

  def queryColName(view: ViewDef, f: FieldDef): String =
    Option(f.alias).getOrElse(
      if (isI18n(f)) f.name else queryColTableAlias(view, f) + "." + f.name)

  def cols(view: ViewDef, countAll: Boolean,
    pathToAlias: Map[List[String], String],
    fieldFilter: FieldFilter,
  ): String =
    if (countAll) " {count(*)}"
    else view.fields
      .filter(f => !f.isExpression || f.expression != null)
      .filter(f => !f.isCollection ||
        !f.type_.isComplexType     ||
        (f.type_.isComplexType && !countAll && !f.isExpression))
      .filter(f => fieldFilter == null || !isOptionalField(f) || fieldFilter.shouldInclude(f.fieldName))
      .map(f => queryColExpression(view, f, pathToAlias, fieldFilter)
        + Option(queryColAlias(f)).map(" " + _).getOrElse(""))
      .mkString(" {", ", ", "}")
  def groupBy(view: ViewDef): String = Option(view.groupBy)
    .map(_ mkString ", ")
    .filter(_ != "").map(g => s"($g)") getOrElse ""
  def having(view: ViewDef): String = Option(view.having)
    .map(hl => if (hl.lengthCompare(1) > 0) hl.map(h => s"($h)") else hl)
    .map(_ mkString " & ")
    .filter(_ != "").map(h => s"^($h)") getOrElse ""
  def fromAndPathToAlias(view: ViewDef): (String, Map[List[String], String]) =
    fromAndPathToAlias(view, view.fields)
  private def fromAndPathToAlias(view: ViewDef, view_fields: Seq[FieldDef]): (String, Map[List[String], String]) = {
    try fromAndPathToAliasUnhandled(view, view_fields)
    catch {
      case NonFatal(ex) => throw new RuntimeException("Failed to build query for " + view.name, ex)
    }
  }
  private def fromAndPathToAliasUnhandled(view: ViewDef, view_fields: Seq[FieldDef]): (String, Map[List[String], String]) = {
    // TODO prepare (pre-compile) views, use pre-compiled as source!
    // TODO complain if bad paths (no refs or ambiguous refs) etc.
    def simpleName(name: String) = if (name == null) null else name.lastIndexOf('.') match {
      case -1 => name
      case  i => name.substring(i + 1)
    }
    def tailists[B](l: List[B]): List[List[B]] =
      if (l.isEmpty) Nil else l :: tailists(l.tail)
    val (needsBaseTable, parsedJoins) =
      Option(view.joins)
        .map(joins =>
          Try(joinsParser(view.db, null, joins)).toOption
            .map(joins => (false, joins))
            .getOrElse((true, joinsParser(view.db, tableAndAlias(view), joins))))
        .getOrElse((false, Nil))
    val joinAliasToTables: Map[String, Set[String]] =
      parsedJoins.map(j => Option(j.alias).getOrElse(j.table) -> j.table).toSet
        .filter(_._1 != null)
        .flatMap { case (n, t) => tailists(n.split("\\.").toList).map(_.mkString(".") -> t) }
        .groupBy(_._1)
        .map { kkv => kkv._1 -> kkv._2.map(_._2).toSet }
    val joinedAliases = joinAliasToTables.keySet
    val baseQualifier = baseFieldsQualifier(view)
    val baseTableOrAlias = Option(baseQualifier) getOrElse simpleName(view.table)
    val autoBaseAlias =
      if (baseTableOrAlias == view.table) null else baseTableOrAlias
    val autoBase =
      if (view.tableAlias != null && !needsBaseTable &&
        parsedJoins.exists(_.alias == view.tableAlias)) null
      else if (view.tableAlias == null && !needsBaseTable &&
        parsedJoins.lengthCompare(0) > 0) null // TODO ?
      else List(view.table, autoBaseAlias).filter(_ != null) mkString " "
    val pathToAlias = collection.mutable.Map[List[String], String]()
    pathToAlias ++= joinedAliases.map(a => List(a) -> a).toMap
    val usedNames = collection.mutable.Set[String]()
    usedNames ++= joinedAliases
    val aliasToTable = collection.mutable.Map[String, String]()
    if (baseQualifier != null) {
      pathToAlias += (List(baseQualifier) -> baseQualifier)
      usedNames += baseQualifier
      if (view.table != null)
        // FIXME exclude clashing simple names from different qualified names!
        aliasToTable += (baseQualifier -> view.table)
        aliasToTable += (simpleName(baseQualifier) -> view.table)
    }
    aliasToTable ++= joinAliasToTables.filter(_._2.size == 1).map { case (n, t) => n -> t.head }
    def isExpressionOrPath(f: FieldDef) =
      (f.isExpression || f.expression != null) &&
        Option(f.expression).forall(expr =>
          !FieldRefRegexp.pattern.matcher(expr).matches)
    import parser.Traverser
    val IdentifierExtractor: Traverser[List[List[String]]] = {
      identifiers => {
        case i: Ident => i.ident :: identifiers
      }
    }
    val tablePathsInSimpleFields = view_fields
      .filterNot(isExpressionOrPath)
      .map(f => Option(f.tableAlias) getOrElse {
        if (f.table == view.table) baseQualifier else f.table
      })
      .filterNot(_ == null)
      .map(t => List(t))
      .toSet
    val tablePathsInFieldExpressions = view_fields
      .filter(isExpressionOrPath)
      .map(_.expression)
      .filter(_ != null)
      .filter(_.trim != "")
      .map(parser.parseExp)
      // do not collect identifiers from subqueries (skip deeper analysis)
      .map(parser.transformer({ case q: QueryParser_Query => Null }))
      .flatMap(x => parser.traverser(IdentifierExtractor)(Nil)(x))
      .filter(_.lengthCompare(1) > 0)
      .map(_ dropRight 1)
      .toSet
    val tablePaths = tablePathsInSimpleFields ++ tablePathsInFieldExpressions
    val tablePathsNotJoined = {
      def isTableOrAliasInScope(tableOrAlias: String) =
        joinedAliases.contains(tableOrAlias)                                  ||
        baseTableOrAlias == tableOrAlias                                      ||
        tableMetadata.aliasedRef(view.table, tableOrAlias, view.db).isDefined ||
        tableMetadata.tableDefOption(tableOrAlias, view.db).isDefined
      val qualifiedTablePaths = tablePaths map { parts =>
        (1 to parts.length - 1).find { i =>
          isTableOrAliasInScope(parts.take(i).mkString("."))
        } match {
          case None    => parts
          case Some(1) => parts
          case Some(i) => parts.take(i).mkString(".") :: parts.drop(i)
        }
      }
      val qualifiedTablePathsAndPrePaths =
        qualifiedTablePaths.flatMap(p => tailists(p.reverse).map(_.reverse))
      (qualifiedTablePathsAndPrePaths -- joinedAliases.map(List(_)) - List(baseTableOrAlias))
        .toList.sortBy(p => (p.size, p mkString "."))
    }
    tablePathsNotJoined foreach { path =>
      val name = (path takeRight 1).head
      val alias = unusedName(name, usedNames)
      pathToAlias += path -> alias
      usedNames += alias
    }
    val autoJoins = tablePathsNotJoined.map { path =>
      val (contextTable, contextTableOrAlias) =
        if (path.lengthCompare(1) == 0) (view.table, baseTableOrAlias)
        else {
          val contextTableOrAlias = pathToAlias(path dropRight 1)
          val contextTable = aliasToTable(contextTableOrAlias)
          (contextTable, contextTableOrAlias)
        }
      val tableOrAlias = path.last
      val alias = pathToAlias(path)
      // TODO inner join if all steps in path to root are not nullable
      val shouldOuterJoin = true
      val joinMod = if (shouldOuterJoin) "?" else ""
      tableMetadata.aliasedRef(contextTable, tableOrAlias, view.db) match {
        // FIXME support multi-col refs
        case Some(ref) =>
          aliasToTable += alias -> ref.refTable
          s"$contextTableOrAlias[$contextTableOrAlias.${ref.cols(0)} $alias$joinMod] ${ref.refTable}"
        case None =>
          aliasToTable += alias -> tableOrAlias
          contextTableOrAlias + "/" + tableOrAlias + joinMod +
            (if (tableOrAlias == alias) "" else " " + alias)
      }
    }
    val joinsString =
      TresqlJoinsParser.joinsString(
        autoBase,
        List(Option(view.joins).getOrElse(Nil), autoJoins).flatten
          .filter(_ != null).filter(_ != "")
      )
    (joinsString, pathToAlias.toMap)
  }
  /*
  val where = (filter.map(f =>
    queryColExpression(fieldNameToDef(f._2.Field)) + " " + comparison(f._2.Comparison) +
      " :" + f._1) ++ Option(extraFilter).filter(_ != ""))
    .mkString("[", " & ", "]") match { case "[]" => "" case a => a }
  */
  def where(view: ViewDef, extraFilter: String, pathToAlias: Map[List[String], String] = null): String =
    (Option(view.filter).getOrElse(Nil) ++ Option(extraFilter))
      .filter(_ != null).filter(_ != "")
      .map(transformFilter(_, view, baseFieldsQualifier(view), pathToAlias))
      .map("[" + _ + "]").mkString match { case "" => "" case a => a }

  def order(view: ViewDef, orderBy: String): String =
    Option(orderBy).orElse(Option(view.orderBy).map(_ mkString ", "))
    .filter(_ != "").map(o => s"#($o)") getOrElse ""

  /* FIXME
    if (countAll || sort == null || sort.size == 0) ""
    else sort.map(s => (if (s.Order == "desc" || s.Order == "desc null") "~" else "") +
      (fieldNameToDef(s.Field) match {
        case f if f.isExpression && f.expression != null =>
          f.expression
        case f if isRu(f) =>
          "NLSSORT(" + queryColName(f) + ", 'NLS_SORT = RUSSIAN')"
        case f => queryColName(f)
      }) + (if (Option(s.Order).getOrElse("") endsWith " null") " null" else "")).mkString("#(", ", ", ")")
  */

  def limitOffset(query: String, countAll: Boolean, limit: Int, offset: Int
      ): (String, Array[Int]) = (if (countAll) (0, 0) else (limit, offset)) match {
    case (0, 0) => (query, Array())
    case (limit, 0) =>
      (query + "@(?)", Array(limit))
    case (0, offset) =>
      (query + "@(?,)", Array(offset))
    case (limit, offset) =>
      (query + "@(? ?)", Array(offset, limit))
  }

  def tableAndAlias(view: ViewDef): String =
    Option(view.table)
      .map(_ + Option(view.tableAlias).map(" " + _).getOrElse(""))
      .orNull
/*
  val values = if (filter == null) Map[String, Any]() else filter.map(f => {
    val v = f._2.Value
    // TODO describe convertion error (field, table, value, ...)
    // TODO extract filter type convertion to filter map for overrides
    f._1 -> (fieldNameToDef(f._2.Field).type_.name match {
      case "string" => v
      case "int" => v.toInt
      case "long" => v.toLong
      case "integer" => BigInt(v)
      case "decimal" => BigDecimal(v)
      /*
       * TODO
      case "date" => Format.xsdDate.parse(v)
      case "dateTime" => Format.xsdDateTime.parse(v)
      */
      case "boolean" => v match {
        case "true" | "TRUE" => "Y"
        case "false" | "FALSE" => "N"
        case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
      }
      case x => sys.error("Filter value type not supported: " + x)
    })
  }).toMap
*/
}

trait OracleQueryStringBuilder extends QueryStringBuilder {
  this: QuereaseMetadata with QuereaseExpressions with FilterTransformer with QuereaseResolvers =>
  override def limitOffset(query: String, countAll: Boolean, limit: Int, offset: Int) =
    if (countAll || limit == 0 || offset == 0)
      super.limitOffset(query, countAll, limit, offset)
    else // ora specific!
      // use limit + offset instead of limit because ora dialect not ready
      (query + "@(? ?)", Array(offset, limit + offset))
}
