package querease

import org.tresql.Result
import org.tresql.RowLike
import mojoz.metadata.Naming
import mojoz.metadata.Type
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.TableDef.TableDefBase
import mojoz.metadata.ColumnDef.ColumnDefBase
import mojoz.metadata.TableMetadata

trait QuereaseMetadata { this: Querease =>

  type FieldDef <: FieldDefBase[Type]
  type ViewDef <: ViewDefBase[FieldDef]

  class FieldOrdering(val nameToIndex: Map[String, Int]) extends Ordering[String] {
    override def compare(x: String, y: String) =
      nameToIndex.get(x).getOrElse(999) - nameToIndex.get(y).getOrElse(999)
  }
  object FieldOrdering {
    def apply(view: ViewDef) =
      new FieldOrdering(view.fields.map(f => Option(f.alias) getOrElse f.name).zipWithIndex.toMap)
  }

  def tableMetadata: TableMetadata[TableDefBase[ColumnDefBase[Type]]]
  def nameToExtendedViewDef: Map[String, ViewDef]
  def fieldOrdering(viewName: String): Ordering[String]

  def tableDef(tableName: String): TableDefBase[ColumnDefBase[Type]] =
    tableMetadata.tableDef(tableName)
  def viewDef(viewName: String) = {
    nameToExtendedViewDef.get(viewName)
      .getOrElse(sys.error(s"View definition for ${viewName} not found"))
  }
  def viewDefOption(viewName: String): Option[ViewDef] = nameToExtendedViewDef.get(viewName)
  def classToViewName(viewClass: Class[_ <: AnyRef]): String =
    ViewName.get(viewClass).replace("-", "_")
  def viewDef(viewClass: Class[_ <: AnyRef]): ViewDef =
    viewDefOption(classToViewName(viewClass)).getOrElse(
      sys.error(s"View definition for ${classToViewName(viewClass)} (for class ${viewClass.getName}) not found"))
  def fieldOrdering(viewClass: Class[_ <: AnyRef]): Ordering[String] =
    fieldOrdering(classToViewName(viewClass))
}

object ViewName {
  def get(viewClass: Class[_ <: AnyRef]) =
    Naming.dasherize(viewClass.getSimpleName)
}
