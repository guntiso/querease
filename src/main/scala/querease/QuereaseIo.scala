package querease

import org.tresql.Result

import mojoz.metadata.DbConventions
import mojoz.metadata.Type
import mojoz.metadata.ViewDef

trait QuereaseIo {
  def toMap(instance: AnyRef): Map[String, _]
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef[Type]): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef[Type] // TODO not class
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    DbConventions.dasherize(viewClass.getSimpleName)
}
