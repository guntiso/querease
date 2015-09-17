package querease

import org.tresql.Result
import mojoz.metadata.Naming
import mojoz.metadata.Type
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.TableDef
import mojoz.metadata.ColumnDef
import mojoz.metadata.TableMetadata

trait QuereaseIo {

  type FieldDef <: FieldDefBase[Type]
  type ViewDef <: ViewDefBase[FieldDef]

  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
  def getKeyMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
  def tableMetadata: TableMetadata[TableDef[ColumnDef[Type]]]
  def getViewDef(viewName: String): ViewDef
  def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef =
    getViewDef(ViewName.get(viewClass).replace("-", "_"))
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    Naming.dasherize(viewClass.getSimpleName)
}
