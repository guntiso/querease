package querease

import org.tresql.Result
import mojoz.metadata.DbConventions
import mojoz.metadata.Type
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.ColumnDef

trait QuereaseIo {
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]): Map[String, _]
  def getKeyMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef[FieldDef[Type]] // TODO not class
}

object QuereaseIo {
  def scalaDto(nameToExtendedViewDef: Map[String, ViewDef[FieldDef[Type]]]): QuereaseIo =
    new ScalaDtoQuereaseIo(nameToExtendedViewDef)
  def jaxbPojo(nameToExtendedViewDef: Map[String, ViewDef[FieldDef[Type]]]): QuereaseIo =
    new JaxbPojoQuereaseIo(nameToExtendedViewDef)
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    DbConventions.dasherize(viewClass.getSimpleName)
}
