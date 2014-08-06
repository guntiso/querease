package querease

import java.lang.reflect.ParameterizedType

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.seqAsJavaList
import scala.language.postfixOps

import org.tresql.Result

import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }

import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar
import mojoz.metadata.DbConventions.xsdNameToDbName
import mojoz.metadata._

private[querease] class JaxbPojoQuereaseIo(nameToExtendedViewDef: Map[String, ViewDef[FieldDef[Type]]]) extends QuereaseIo {

  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance

  override def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef[FieldDef[Type]] =
    // FIXME apply naming properly
    nameToExtendedViewDef.get(ViewName.get(viewClass)) getOrElse
      (nameToExtendedViewDef.get(ViewName.get(viewClass)
        .replace("-", "_")) getOrElse
        (viewClass.getSuperclass match {
          case c: Class[_] =>
            try getViewDef(c.asInstanceOf[Class[_ <: AnyRef]]) catch {
              case e: Exception => throw new RuntimeException(
                "Failed to get view definition for " + viewClass.getName, e)
            }
          case x => throw new RuntimeException(
            "Failed to get view definition for " + viewClass.getName)
        }))
  def pojoToMap(pojo: Any): Map[String, _] = {
    def propName(m: java.lang.reflect.Method) = {
      val mName = m.getName
      if (mName.startsWith("get") && mName.length > 3 &&
        mName.charAt(3).isUpper) mName.substring(3)
      else if (mName.startsWith("is") && mName.length > 2 &&
        mName.charAt(2).isUpper) mName.substring(2)
      else throw new RuntimeException(
        "Failed to extract property name from method name: " + mName)
    }
    def isPrimitive[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null || (x match {
      case _: java.lang.Number | _: java.lang.Boolean | _: java.util.Date | _: XMLGregorianCalendar => true
      case _ => false
    })
    if (pojo == null) Map.empty
    else pojo.getClass.getMethods filter (m =>
      m.getName.startsWith("get") && m.getName != "getClass"
        || m.getName.startsWith("is")) filter (m =>
      m.getParameterTypes.size == 0) map (m =>
      propName(m) -> (m.invoke(pojo) match {
        case null => null
        case x: String => x
        case c: Class[_] => c
        case x: XMLGregorianCalendar => x.toGregorianCalendar.getTime
        case x if (isPrimitive(x)) => x
        case b: Array[Byte] => new java.io.ByteArrayInputStream(b)
        case l: Seq[_] => l map pojoToMap
        case l: Array[_] => l map pojoToMap
        case l: java.util.Collection[_] => l map pojoToMap
        case x => throw new RuntimeException(
          "Pojo map not implemented - class: " + x.getClass + ", value: " + x)
      })) toMap
  }

  override def fromRows[T <: AnyRef](rows: Result, pojoClass: Class[T]) = {
    def toPojo(m: Map[String, Any]) = mapToPojo(m, pojoClass.newInstance)
    rows.toListOfMaps map toPojo
  }
  def mapToPojo[T](m: Map[String, _], pojo: T): T = {
    def propToClassName(prop: String) =
      if (prop.endsWith("List")) prop.dropRight(4) else prop
    def getCollectionType(t: java.lang.reflect.Type) = {
      val parametrisedType = t.asInstanceOf[ParameterizedType]
      parametrisedType.getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]];
    }
    def lowerNames(m: Map[String, Any]) = m.map(e => (e._1.toLowerCase, e._2))
    val map = lowerNames(m)
    pojo.getClass.getMethods.filter(m => m.getName.startsWith("set") &&
      m.getParameterTypes.size == 1) foreach { m =>
      val propName = m.getName.drop(3) //property name
      val propClass = propToClassName(propName) //find class name in the case value is map or list i.e. not primitive object
      val t = m.getParameterTypes()(0)

      map.get(xsdNameToDbName(propName)).map(value => try { // FIXME wtf rename propname?
        m.invoke(pojo, convertValue(value, t, propClass))
      } catch {
        case ex: Exception =>
          throw new RuntimeException("Failed to invoke setter " + m.getName +
            "(" + t.getName + ") with value " + value +
            " of class " + (Option(value).map(_.getClass.getName) getOrElse "?"), ex)
      })
    }
    // collection part, supports now only list of maps->to list of pojos part
    pojo.getClass.getMethods.filter(m =>
      m.getName.startsWith("get") &&
        classOf[java.util.Collection[_]].isAssignableFrom(m.getReturnType) &&
        m.getParameterTypes.size == 0)
      .foreach { m =>
        val propName = m.getName.drop(3) //property name
        val genericType = getCollectionType(m.getGenericReturnType)
        map.get(xsdNameToDbName(propName)).foreach { mapElement =>
          mapElement match {
            case list: List[_] =>
              val collection = m.invoke(pojo).asInstanceOf[java.util.Collection[java.lang.Object]]
              collection.clear
              list.foreach { data =>
                val child = genericType.newInstance.asInstanceOf[java.lang.Object]
                mapToPojo(data.asInstanceOf[Map[String, _]], child)
                collection.add(child)
              }
            case _ =>
          }
        }
      }
    pojo
  }

  def convertValue(value: Any, t: Class[_],
    itemClassName: String = "<collections not supported>"): AnyRef = value match {
    case d: BigDecimal => {
      if (t == classOf[Int] || t == classOf[java.lang.Integer])
        new java.lang.Integer(d.toInt)
      else if (t == classOf[Long] || t == classOf[java.lang.Long])
        new java.lang.Long(d.toLong)
      else if (t == classOf[Double] || t == classOf[java.lang.Double])
        d.doubleValue.asInstanceOf[Object]
      else if (t == classOf[java.math.BigDecimal])
        d.bigDecimal
      else if (t == classOf[java.math.BigInteger])
        d.bigDecimal.unscaledValue
      else d
    }
    case i: Integer => {
      if (t == classOf[Int] || t == classOf[java.lang.Integer])
        i
      else if (t == classOf[Long] || t == classOf[java.lang.Long])
        new java.lang.Long(i.toLong)
      else if (t == classOf[Double] || t == classOf[java.lang.Double])
        i.doubleValue.asInstanceOf[Object]
      else if (t == classOf[java.math.BigDecimal])
        new java.math.BigDecimal(i.intValue())
      else if (t == classOf[java.math.BigInteger])
        java.math.BigInteger.valueOf(i.toLong)
      else if (t == classOf[String] || t == classOf[java.lang.String])
        i.toString
      else i
    }
    case l: java.lang.Long => {
      if (t == classOf[Int] || t == classOf[java.lang.Integer])
        new java.lang.Integer(l.toInt)
      else if (t == classOf[Long] || t == classOf[java.lang.Long])
        l
      else if (t == classOf[Double] || t == classOf[java.lang.Double])
        l.doubleValue.asInstanceOf[Object]
      else if (t == classOf[java.math.BigDecimal])
        new java.math.BigDecimal(l.longValue)
      else if (t == classOf[java.math.BigInteger])
        java.math.BigInteger.valueOf(l.toLong)
      else if (t == classOf[String] || t == classOf[java.lang.String])
        l.toString
      else l
    }
    case x if (t == classOf[java.math.BigInteger] && x != null) =>
      new java.math.BigInteger(x.toString)
    case x: java.util.Date if (t == classOf[XMLGregorianCalendar]) => {
      val gc = new java.util.GregorianCalendar()
      gc.setTime(x)
      XML_DATATYPE_FACTORY.newXMLGregorianCalendar(gc)
    }
    case inMap: Map[_, _] =>
      mapToPojo(inMap.asInstanceOf[Map[String, _]],t.newInstance).asInstanceOf[Object]
    case List(inMap: Map[_, _]) =>
      mapToPojo(inMap.asInstanceOf[Map[String, _]],t.newInstance).asInstanceOf[Object]
    case Nil => null
    //may be exact collection which is used in xsd generated pojos must be used?
    case Seq() if (classOf[java.util.Collection[_]].isAssignableFrom(t)) =>
      t.newInstance.asInstanceOf[java.util.Collection[_]]
    case s: Seq[_] if (classOf[java.util.Collection[_]].isAssignableFrom(t)) => {
      val col: java.util.Collection[_] = s.asInstanceOf[Seq[Map[String, _]]]
        .map(mapToPojo(_, Class.forName(itemClassName).newInstance))
      col
    }
    case x: String if t == classOf[Boolean] || t == classOf[java.lang.Boolean] => x match {
      case "y" | "Y" | "true" | "TRUE" => java.lang.Boolean.TRUE
      case "n" | "N" | "false" | "FALSE" => java.lang.Boolean.FALSE
      case null => java.lang.Boolean.FALSE
      case x => sys.error("No idea how to convert to boolean: \"" + x + "\"")
    }
    case blob: java.sql.Blob if t == classOf[Array[Byte]] =>
      blob.getBytes(1, blob.length.toInt) // FIXME toInt!
    case x => x.asInstanceOf[Object]
  }

  def xsdValueToDbValue(xsdValue: Any) = xsdValue match {
    case true => "Y"
    case false => "N"
    /*
    // avoid unfriendly oracledb error message
    case x: String if x.length > 1000 && x.getBytes("UTF-8").length > 4000 =>
      err(TEXT_TOO_LONG, "" + x.getBytes("UTF-8").length)
    */
    case x => x
  }

  def toPlural(s: String) = // comply with JAXB plural
    if (s.endsWith("y")) s.dropRight(1) + "ies"
    else if (s endsWith "tus") s + "es"
    else if (s endsWith "apiks") s
    else s + "s"

  def toSingular(s: String) = // XXX to undo JAXB plural
    if (s endsWith "ies") s.dropRight(3) + "y"
    else if (s endsWith "tuses") s.dropRight(2)
    else if (s endsWith "apiks") s
    else if (s endsWith "s") s.dropRight(1)
    else s

  override def toSaveableMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]) =
    pojoToSaveableMap(instance, viewDef)
  override def getKeyMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]) =
    // FIXME when key != id, use viewDef to get key-values if defined
    Map("id" -> getId(instance))
  def pojoToSaveableMap(pojo: AnyRef, viewDef: ViewDef[FieldDef[Type]]) = {
    def toDbFormat(m: Map[String, _]): Map[String, _] = m.map {
      case (k, vList: List[Map[String, _]]) =>
        (xsdNameToDbName(k), vList map toDbFormat)
      case (k, v) => (xsdNameToDbName(k), xsdValueToDbValue(v))
    }
    val propMap = toDbFormat(pojoToMap(pojo))
    def trim(value: Any) = value match {
      case s: String => s.trim()
      case x => x
    }

    def toSaveableDetails(propMap: Map[String, Any], viewDef: ViewDef[FieldDef[Type]]): Map[String, Any] = {
      def getChildViewDef(viewDef: ViewDef[_], fieldDef: FieldDef[Type]) =
        nameToExtendedViewDef.getOrElse(fieldDef.type_.name,
          sys.error("Child viewDef not found: " + fieldDef.type_.name +
            " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))
      def isSaveable(f: FieldDef[Type]) = !f.isExpression
      def getFieldDef(fieldName: String) =
        viewDef.fields.find(f =>
          Option(f.alias).getOrElse(f.name) == fieldName).getOrElse(sys.error(
          "Field not found for property: " + viewDef.name + "." + fieldName))
      propMap.filter(_._1 != "clazz").map {
        case (key, l: List[Map[String, _]]) =>
          val fieldName = toSingular(key) // XXX undo JAXB plural 
          val fieldDef = getFieldDef(fieldName)
          if (isSaveable(fieldDef)) {
            val childViewDef = getChildViewDef(viewDef, fieldDef)
            childViewDef.table -> l.map(toSaveableDetails(_, childViewDef))
          } else ("!" + key, l)
        case (key, value) =>
          val fieldName = key
          val fieldDef = getFieldDef(fieldName)
          if (isSaveable(fieldDef))
            if (fieldDef.type_.isComplexType) {
              val childViewDef = getChildViewDef(viewDef, fieldDef)
              childViewDef.table -> toSaveableDetails(
                value.asInstanceOf[Map[String, Any]], childViewDef)
            } else (key, value)
          else ("!" + key, value)
      }
    }
    toSaveableDetails(propMap, viewDef)
      .filter(e => !(e._1 startsWith "!"))
      .map(e => (e._1, trim(e._2)))
  }
  private val NoArgs = Array[Class[_]]()
  private def getId(pojo: AnyRef): java.lang.Long = {
    val getter = pojo.getClass().getMethod("getId", NoArgs: _*)
    getter.invoke(pojo).asInstanceOf[java.lang.Long]
  }
}
