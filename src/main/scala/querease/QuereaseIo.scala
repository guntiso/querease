package querease

import metadata.XsdTypeDef

trait QuereaseIo {
  def toMap(instance: AnyRef): Map[String, _]
  def fromMap[T <: AnyRef](map: Map[String, _], instance: T): T
  def toSaveableMap(instance: AnyRef, viewDef: XsdTypeDef): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): XsdTypeDef // TODO not class
}
