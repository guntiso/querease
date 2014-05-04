package querease

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

private[querease] class ScalaDtoQuereaseIo(metadata: Metadata[Type]) extends QuereaseIo {
  override def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]) = {
    def toDto(r: RowLike) = {
      val t = clazz.newInstance
      t.asInstanceOf[Dto].fill(r)
      t
    }
    rows.map(toDto).toList
  }
  override def toSaveableMap(instance: AnyRef, viewDef: ViewDef[Type]) =
    instance.asInstanceOf[Dto].toSaveableMap
  override def getViewDef(viewClass: Class[_ <: AnyRef]) = {
    metadata.extendedViewDef.get(ViewName.get(viewClass).replace("-", "_"))
      .getOrElse(sys.error(s"View definition for ${viewClass.getName} not found"))
  }
}

  //retrieving from tresql plain objects with public var fields
  object Dto {
    implicit def rowLikeToDto[T <: Dto](r: RowLike, m: Manifest[T]): T =
      m.runtimeClass.newInstance.asInstanceOf[T].fill(r)

    //dto to map for ORT
    implicit def dtoToMap[T <: Dto](p: T): (String, Map[String, Any]) =
      "TODO" -> p.toSaveableMap // TODO convert class to table name
      /*
      model.Metadata.dtoClassToTable(p.getClass) -> p.toSaveableMap
      */
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

    def toMap: Map[String, Any] = setters.flatMap { m =>
      scala.util.Try(getClass.getMethod(m._1).invoke(this)).toOption.map {
        case s: Seq[Dto] => m._1 -> (s map (_.toMap))
        case x => m._1 -> x
      }
    } toMap
    
    //creating map from object
    def toSaveableMap: Map[String, Any] =
      (setters.toList.flatMap { m =>
        scala.util.Try(getClass.getMethod(m._1).invoke(this)).toOption.map {
          //objects from one list can be put into different tables
          case s: Seq[Dto] => s map Dto.dtoToMap groupBy (_._1) map (t => t._1 -> (t._2 map (_._2))) toList
          case x => List(m._1 -> x)
        } getOrElse Nil
        //child objects from different lists can be put into one table
      }).groupBy { case (_, _: Seq[_]) => "s" case _ => "v" } flatMap {
        case ("s", l: Seq[(_, Seq[_])]) =>
          l groupBy (_._1) map (t => t._1 -> (t._2.asInstanceOf[Seq[(_, Seq[_])]] flatMap (_._2)))
        case ("v", x) => x
      }
  }

  trait DtoWithId extends Dto {
    def id: java.lang.Long
    def id_=(id: java.lang.Long)
  }
