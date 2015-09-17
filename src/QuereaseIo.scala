package querease

import org.tresql.Result
import mojoz.metadata.Naming
import mojoz.metadata.Type
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.TableDef
import mojoz.metadata.ColumnDef
import mojoz.metadata.TableMetadata

trait QuereaseIo {
  def fromRows[T <: AnyRef](rows: Result, clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]): Map[String, _]
  def getKeyMap(instance: AnyRef, viewDef: ViewDef[FieldDef[Type]]): Map[String, _]
  def tableMetadata: TableMetadata[TableDef[ColumnDef[Type]]]
  def getViewDef(viewName: String): ViewDef[FieldDef[Type]]
  def getViewDef(viewClass: Class[_ <: AnyRef]): ViewDef[FieldDef[Type]] =
    getViewDef(ViewName.get(viewClass).replace("-", "_"))
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    Naming.dasherize(viewClass.getSimpleName)
}
