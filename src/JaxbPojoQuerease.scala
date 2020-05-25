package querease

import java.lang.reflect.ParameterizedType

import scala.collection.JavaConverters._
import scala.language.postfixOps

import org.tresql.RowLike

import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar

trait JaxbPojoQuereaseIo extends QuereaseIo { this: Querease =>

  val XML_DATATYPE_FACTORY = DatatypeFactory.newInstance

  def xsdNameToDbName(name: String) = name
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
      m.getParameterTypes.length == 0) map (m =>
      propName(m) -> (m.invoke(pojo) match {
        case null => null
        case x: String => x
        case c: Class[_] => c
        case x: XMLGregorianCalendar => x.toGregorianCalendar.getTime
        case x if isPrimitive(x) => x
        case b: Array[Byte] => new java.io.ByteArrayInputStream(b)
        case l: Seq[_] => l map pojoToMap
        case l: Array[_] => l map pojoToMap
        case l: java.util.Collection[_] => l.asScala map pojoToMap
        case x => throw new RuntimeException(
          "Pojo map not implemented - class: " + x.getClass + ", value: " + x)
      })) toMap
  }

  override def convertRow[B <: DTO](row: RowLike)(implicit mf: Manifest[B]) =
    mapToPojo(row.toMap, mf.runtimeClass.newInstance.asInstanceOf[B])
  def mapToPojo[B <: DTO: Manifest](m: Map[String, _], pojo: B): B = {
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
        m.invoke(pojo, convertValue(value, propClass))
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
        m.getParameterTypes.length == 0)
      .foreach { m =>
        val propName = m.getName.drop(3) //property name
        val genericType = getCollectionType(m.getGenericReturnType)
        map.get(xsdNameToDbName(propName)).foreach {
          case list: List[_] =>
            val collection = m.invoke(pojo).asInstanceOf[java.util.Collection[java.lang.Object]]
            collection.clear
            list.foreach { data =>
              val child = genericType.newInstance.asInstanceOf[B]
              mapToPojo(data.asInstanceOf[Map[String, _]], child)
              collection.add(child)
            }
          case _ =>
        }
      }
    pojo
  }

  def convertValue[B <: DTO](value: Any,
      itemClassName: String = "<collections not supported>")(implicit mf: Manifest[B]): AnyRef = {
    val t = mf.runtimeClass
    value match {
      case d: BigDecimal => {
        if (t == classOf[Int] || t == classOf[java.lang.Integer])
          new java.lang.Integer(d.toInt)
        else if (t == classOf[Long] || t == classOf[java.lang.Long])
          java.lang.Long.valueOf(d.toLong)
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
          java.lang.Long.valueOf(i.toLong)
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
      case x if t == classOf[java.math.BigInteger] && x != null =>
        new java.math.BigInteger(x.toString)
      case x: java.util.Date if t == classOf[XMLGregorianCalendar] => {
        val gc = new java.util.GregorianCalendar()
        gc.setTime(x)
        XML_DATATYPE_FACTORY.newXMLGregorianCalendar(gc)
      }
      case inMap: Map[_, _] =>
        mapToPojo(inMap.asInstanceOf[Map[String, _]],t.newInstance.asInstanceOf[B])
      case List(inMap: Map[_, _]) =>
        mapToPojo(inMap.asInstanceOf[Map[String, _]],t.newInstance.asInstanceOf[B])
      case Nil => null
      //may be exact collection which is used in xsd generated pojos must be used?
      case Seq() if classOf[java.util.Collection[_]].isAssignableFrom(t) =>
        t.newInstance.asInstanceOf[java.util.Collection[_]]
      case s: Seq[_] if classOf[java.util.Collection[_]].isAssignableFrom(t) => {
        val col: java.util.Collection[_] = s.asInstanceOf[Seq[Map[String, _]]]
          .map(mapToPojo(_, Class.forName(itemClassName).newInstance.asInstanceOf[B])).asJava
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

  override def toSaveableMap[B <: DTO](instance: B) = pojoToSaveableMap(instance)
  override def keyMap[B <: DTO](instance: B) =
    // FIXME when key != id, use viewDef to get key-values if defined
    Map("id" -> getId(instance))
  def pojoToSaveableMap[B <: DTO](pojo: B) = {
    def toDbFormat(m: Map[String, _]): Map[String, _] = m.map {
      case (k, vList: List[_]) =>
        (xsdNameToDbName(k), vList.asInstanceOf[List[Map[String, _]]] map toDbFormat)
      case (k, v) => (xsdNameToDbName(k), xsdValueToDbValue(v))
    }
    val propMap = toDbFormat(pojoToMap(pojo))
    def trim(value: Any) = value match {
      case s: String => s.trim()
      case x => x
    }

    def toSaveableDetails(propMap: Map[String, Any], viewDef: ViewDef): Map[String, Any] = {
      def getChildViewDef(viewDef: ViewDef, fieldDef: FieldDef) =
        viewDefOption(fieldDef.type_.name).getOrElse(
          sys.error("Child viewDef not found: " + fieldDef.type_.name +
            " (referenced from " + viewDef.name + "." + fieldDef.name + ")"))
      def isSaveable(f: FieldDef) = !f.isExpression
      def getFieldDef(fieldName: String) =
        viewDef.fields.find(f =>
          Option(f.alias).getOrElse(f.name) == fieldName).getOrElse(sys.error(
          "Field not found for property: " + viewDef.name + "." + fieldName))
      propMap.filter(_._1 != "clazz").map {
        case (key, list: List[_]) =>
          val l = list.asInstanceOf[List[Map[String, _]]]
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
