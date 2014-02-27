package querease

import org.tresql.Result

import metadata.XsdTypeDef

trait QuereaseIo {
  def toMap(instance: AnyRef): Map[String, _]
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: XsdTypeDef): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): XsdTypeDef // TODO not class
}
