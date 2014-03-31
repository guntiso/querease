package querease

import org.tresql.Result

import mojoz.metadata.DbConventions
import mojoz.metadata.XsdTypeDef

trait QuereaseIo {
  def toMap(instance: AnyRef): Map[String, _]
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: XsdTypeDef): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): XsdTypeDef // TODO not class
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    DbConventions.dasherize(viewClass.getSimpleName)
}
