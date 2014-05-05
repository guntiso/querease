package querease

import org.tresql.Result
import mojoz.metadata.DbConventions
import mojoz.metadata.Type
import mojoz.metadata.ViewDef
import mojoz.metadata.FieldDef
import mojoz.metadata.ColumnDef
import mojoz.metadata.Metadata

trait QuereaseIo {
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef[Type]): Map[String, _]
  def getKeyMap(instance: AnyRef, viewDef: ViewDef[Type]): Map[String, _]
  def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef[Type] // TODO not class
}

object QuereaseIo {
  def scalaDto(metadata: Metadata[Type]): QuereaseIo =
    new ScalaDtoQuereaseIo(metadata)
  def jaxbPojo(metadata: Metadata[Type]): QuereaseIo =
    new JaxbPojoQuereaseIo(metadata)
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    DbConventions.dasherize(viewClass.getSimpleName)
}
