package org.uniso

import org.uniso.tus.tresql.CustomFunctions
package object tus {
  import java.sql.Connection
  import org.tresql._
  import com.jolbox.bonecp.BoneCP
  import spray.json._
  import DefaultJsonProtocol._
  import spray.httpx.unmarshalling._
  import spray.httpx.marshalling._
  import scala.util.Try
  import scala.util.Success
  import scala.util.Failure
  import spray.http.HttpHeaders._
  import spray.http._
  import MediaTypes._
  import com.jolbox.bonecp.BoneCP
  import com.jolbox.bonecp.BoneCPConfig
  import scala.collection.JavaConversions._
  import org.fusesource.scalate._
  import com.typesafe.config._
  import scala.reflect.ManifestFactory

  //jade template engine configuration 
  val templateEngine = new TemplateEngine
  templateEngine.layoutStrategy = new layout.DefaultLayoutStrategy(templateEngine, "default.jade")

  //db connection pool configuration
  val config = ConfigFactory.load
  implicit val connectionPool = {
    Class.forName(config.getString("jdbc.driver"))
    val props = new java.util.Properties(System.getProperties)
    for (e <- config.entrySet; if e.getKey.startsWith("bonecp"))
      props.setProperty(e.getKey, config.getString(e.getKey))
    val poolConfig = new BoneCPConfig(props)
    poolConfig setJdbcUrl config.getString("jdbc.url")
    poolConfig setUsername config.getString("jdbc.usr")
    poolConfig setPassword config.getString("jdbc.pwd")
    new BoneCP(poolConfig)
  }

  //tresql environment configuration
  val tresqlDebug = config.getBoolean("tresql.debug")
  Env update ((m, l) => if (tresqlDebug) println(m))
  Env.dialect = dialects.InsensitiveCmp("ĒŪĪĀŠĢĶĻŽČŅēūīāšģķļžčņ", "EUIASGKLZCNeuiasgklzcn") orElse
    dialects.SqlEscapeDialect orElse dialects.OracleDialect
  Env.idExpr = _ => "dual {hibernate_sequence.nextval}"
  Env.functions = CustomFunctions
  Env.cache = new SimpleCache

  private def setenv(pool: BoneCP) {
    Env.conn = pool.getConnection
  }
  private def clearEnv(rollback: Boolean = false) = {
    val conn = Env.conn
    Env.conn = null
    if (rollback && conn != null) try conn.rollback catch { case e: Exception => e.printStackTrace }
    if (conn != null) try if (!conn.isClosed) conn.close catch { case e: Exception => e.printStackTrace }
  }
  def dbUse[A](a: => A)(implicit pool: BoneCP): A = {
    val outer = Env.conn == null
    if (outer) setenv(pool)
    try {
      a
    } finally {
      if (outer) clearEnv(true)
    }
  }
  def transaction[A](a: => A)(implicit pool: BoneCP): A = {
    // FIXME transaction block in use block!
    // FIXME multiple db transaction and use blocks!
    val outer = Env.conn == null
    if (outer) setenv(pool)
    try {
      val res = a
      if (outer) Env.conn.commit()
      res
    } catch {
      case ex: Exception =>
        try Env.conn.rollback catch {
          case e: Exception => e.printStackTrace
        }
        throw ex
    } finally {
      if (outer) clearEnv()
    }
  }

  trait JsonConverter {
    def r(value: JsValue): Any = value match {
      case JsObject(fields) => fields map (f => f._1 -> r(f._2)) toMap
      case JsArray(elements) => elements map r
      case JsString(v) => v
      case JsNumber(v) => v
      case b: JsBoolean => b.value
      case JsNull => null
    }
    def w(value: Any): JsValue = value match {
      case m: Map[String, Any] => JsObject(m map (f => f._1 -> w(f._2)) toMap)
      case l: Traversable[Any] => JsArray(l map w toList)
      case s: String => JsString(s)
      case n: Int => JsNumber(n)
      case n: Long => JsNumber(n)
      case n: Double => JsNumber(n)
      case n: BigInt => JsNumber(n)
      case n: java.lang.Number => JsNumber(String.valueOf(n))
      case b: Boolean => JsBoolean(b)
      case null => JsNull
      case x => JsString(String.valueOf(x))
    }
  }

  implicit object DtoJsonFormat extends RootJsonFormat[Dto] with JsonConverter {
    def read(value: JsValue) = sys.error("not implemented yet!")
    def write(value: Dto) = w(value.toMap)
  }
  implicit object DtoListJsonFormat extends RootJsonFormat[List[Dto]] with JsonConverter {
    def read(value: JsValue) = sys.error("not implemented yet!")
    def write(value: List[Dto]) = w(value map (_.toMap))
  }

  implicit object MapJsonFormat extends JsonFormat[Map[String, Any]] with JsonConverter {
    def read(value: JsValue) = {
      value match {
        case JsObject(fields: Map[String, JsValue]) => r(value).asInstanceOf[Map[String, Any]]
        case x => sys.error("Invalid JsValue object, unable to produce map: " + x)
      }
    }
    def write(value: Map[String, Any]) = {
      w(value).asInstanceOf[JsObject]
    }
  }
  implicit object ListJsonFormat extends JsonFormat[List[Any]] with JsonConverter {
    def read(value: JsValue) = {
      value match {
        case JsArray(elements: List[JsValue]) => r(value).asInstanceOf[List[Any]]
        case x => sys.error("Invalid JsValue object, unable to produce list: " + x)
      }
    }
    def write(value: List[Any]) = {
      w(value).asInstanceOf[JsArray]
    }
  }
  implicit val toResponseDtoMarshaller = Marshaller.of[Dto](`application/json`) {
    (dto, contentType, ctx) => ctx.marshalTo(HttpEntity(contentType, dto.toMap.toJson.prettyPrint))
  }
  implicit val toResponseDtoListMarshaller = Marshaller.of[List[Dto]](`application/json`) {
    (dtoList, contentType, ctx) =>
      ctx.marshalTo(HttpEntity(contentType,
        dtoList.map(_.toMap).toJson.prettyPrint))
  }
  implicit val jsonUnmarshaller: FromRequestUnmarshaller[JsValue] = new Deserializer[HttpRequest, JsValue] {
    def apply(req: HttpRequest) = req.header[`Content-Type`].filter {
      case `Content-Type`(ContentType(`application/json`, _)) => true
      case _ => false
    } map { h =>
      Try(req.entity.asString.asJson) match {
        case Success(json) => Right(json)
        case Failure(err) => Left(MalformedContent(err.getMessage, err))
      }
    } getOrElse Left(UnsupportedContentType(req.header[`Content-Type`] map (_.toString) getOrElse "None"))
  }
  implicit val jsonMapUnmarshaller: FromRequestUnmarshaller[Map[String, Any]] =
    new Deserializer[HttpRequest, Map[String, Any]] {
      def apply(req: HttpRequest) = jsonUnmarshaller.apply(req) match {
        case Right(o: JsObject) => Right(o.convertTo[Map[String, Any]])
        case Right(x) => Left(MalformedContent(x.toString))
        case Left(e) => Left(e)
      }
    }

  implicit val dtoUnmarshaller: FromRequestUnmarshaller[DtoWithId] =
    new Deserializer[HttpRequest, DtoWithId] {
      def apply(req: HttpRequest) = jsonUnmarshaller.apply(req) match {
        case Right(o: JsObject) => req.method match {
          //insert TODO get class name from uri -> class name map
          case HttpMethods.POST => req.uri.path.reverse.head match {
            case s: String =>
              Right(model.Metadata.serviceUriToDtoClass(s).newInstance.asInstanceOf[DtoWithId].fill(o))
          }
          case HttpMethods.PUT => req.uri.path.reverse.tail.tail.head match {
            case s: String =>
              Right(model.Metadata.serviceUriToDtoClass(s).newInstance.asInstanceOf[DtoWithId].fill(o))
          }
        }
        case Right(x) => Left(MalformedContent(x.toString))
        case Left(e) => Left(e)
      }
    }

  implicit val ObjToTable: ORT.ObjToMapConverter[DtoWithId] = (o: DtoWithId) =>
    (model.Metadata.dtoClassToTable(o.getClass), o toMap)

  //retrieving from tresql plain objects with public var fields
  object Dto {
    implicit def rowLikeToDto[T <: Dto](r: RowLike, m: Manifest[T]): T =
      m.runtimeClass.newInstance.asInstanceOf[T].fill(r)

    //dto to map for ORT
    implicit def dtoToMap[T <: Dto](p: T): (String, Map[String, _]) = {
      val n = p.getClass.getName.toLowerCase
      n.substring(n.lastIndexOf("$") + 1) -> p.toMap
    }
  }
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

    def fill(r: RowLike): this.type = {
      for (i <- 0 to (r.columnCount - 1)) r.column(i) match {
        case Column(_, name, _) if name != null => set(name, r)
        case _ =>
      }
      this
    }
    protected def setters = _setters
    protected def set(dbName: String, r: RowLike) =
      for (s <- setters.get(dbToPropName(dbName))) {
        if (s._2._2 != null) { //child result
          val m: Manifest[_ <: Dto] = s._2._2
          val childResult = r.result(dbName)
          s._1.invoke(this, childResult.list[Dto](Dto.rowLikeToDto _,
            m.asInstanceOf[Manifest[Dto]]).asInstanceOf[Object])
        } else s._1.invoke(this, r.typed(dbName)(s._2._1).asInstanceOf[Object])
      }
    protected def dbToPropName(name: String) = name.toLowerCase
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
    //end filling in object from RowLike

    //creating map from object
    def toMap: Map[String, Any] = setters.flatMap { m =>
      scala.util.Try(getClass.getMethod(m._1).invoke(this)).toOption.map {
        case s: Seq[Dto] => m._1 -> (s map (_.toMap))
        case x => m._1 -> x
      }
    } toMap

    //creating dto from JsObject
    def fill(js: JsObject): this.type = {
      js.fields foreach (_ match {
        case (n, v: JsString) =>
          setters.get(n).map(met => {
            if (ManifestFactory.classType(classOf[java.sql.Date]) == met._2._1)
              met._1.invoke(this, new java.sql.Date(new java.text.SimpleDateFormat("yyyy-MM-dd")
              	.parse(v.value).getTime()).asInstanceOf[Object])
            else if (ManifestFactory.classType(classOf[java.sql.Timestamp]) == met._2._1)
              met._1.invoke(this, new java.sql.Timestamp(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
              	.parse(v.value).getTime()).asInstanceOf[Object])  	
            else if (ManifestFactory.singleType(v.value) == met._2._1)
              met._1.invoke(this, v.value.asInstanceOf[Object])
          })
        case (n, v: JsNumber) =>
          setters.get(n).map(met => {
            if (ManifestFactory.singleType(v.value) == met._2._1)
              met._1.invoke(this, v.value.asInstanceOf[Object])
            else if (ManifestFactory.classType(classOf[java.lang.Long]) == met._2._1)
              met._1.invoke(this, v.value.longValue.asInstanceOf[Object])
            else if (ManifestFactory.Long == met._2._1)
              met._1.invoke(this, v.value.longValue.asInstanceOf[Object])
            else if (ManifestFactory.classType(classOf[java.lang.Integer]) == met._2._1)
              met._1.invoke(this, v.value.intValue.asInstanceOf[Object])
            else if (ManifestFactory.Int == met._2._1)
              met._1.invoke(this, v.value.intValue.asInstanceOf[Object])
            else if (ManifestFactory.classType(classOf[java.lang.Double]) == met._2._1)
              met._1.invoke(this, v.value.doubleValue.asInstanceOf[Object])
            else if (ManifestFactory.Double == met._2._1)
              met._1.invoke(this, v.value.doubleValue.asInstanceOf[Object])
          })
        case (n, v: JsBoolean) =>
          setters.get(n).map(met => {
            if (ManifestFactory.classType(classOf[java.lang.Boolean]) == met._2._1)
              met._1.invoke(this, v.value.asInstanceOf[Object])
            else if (ManifestFactory.Boolean == met._2._1)
              met._1.invoke(this, v.value.asInstanceOf[Object])
          })
        case (n, v: JsObject) => setters.get(n).map(met => {
          if (classOf[Dto].isAssignableFrom(met._2._1.runtimeClass)) {
            val o = met._2._1.runtimeClass.newInstance.asInstanceOf[Dto].fill(v)
            met._1.invoke(this, o.asInstanceOf[Object])
          }
        })
        case (n, v: JsArray) => setters.get(n).map(met => {
          val c = met._2._1.runtimeClass
          if (classOf[Seq[_]].isAssignableFrom(c) && c.isAssignableFrom(classOf[List[_]]) &&
            met._2._2 != null && classOf[Dto].isAssignableFrom(met._2._2.runtimeClass)) {
            val chClass = met._2._2.runtimeClass
            println("\n\n" + chClass + "\n\n")
            val l = v.elements.map(
              o => chClass.newInstance.asInstanceOf[Dto].fill(o.asInstanceOf[JsObject]))
            met._1.invoke(this, l.asInstanceOf[Object])
          }
        })
        case (n, JsNull) =>
          setters.get(n).map(met => met._1.invoke(this, null))
      })
      this
    }
  }

  trait DtoWithId extends Dto {
    def id: java.lang.Long
    def id_=(id: java.lang.Long)
  }

}
