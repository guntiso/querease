package org.uniso

package object tus {
  import java.sql.Connection
  import org.tresql.Env
  import com.jolbox.bonecp.BoneCP
  import spray.json._
  import DefaultJsonProtocol._
  import spray.httpx.unmarshalling._
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

}