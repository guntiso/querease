package querease

import org.tresql.Result
import org.tresql.RowLike
import mojoz.metadata._
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.TableDef.TableDefBase
import mojoz.metadata.ColumnDef.ColumnDefBase
import mojoz.metadata.in.{YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}

trait QuereaseMetadata {

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

  protected lazy val yamlMetadata = YamlMd.fromResources()
  lazy val metadataConventions: MdConventions = new SimplePatternMdConventions
  lazy val tableMetadata: TableMetadata[TableDefBase[ColumnDefBase[Type]]] =
    new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions).tableDefs)

  lazy val viewDefs: Map[String, ViewDef] =
    YamlViewDefLoader(tableMetadata, yamlMetadata, TresqlJoinsParser, metadataConventions)
      .extendedViewDefs.asInstanceOf[Map[String, ViewDef]]
  lazy val viewNameToFieldOrdering = viewDefs.map(kv => (kv._1, FieldOrdering(kv._2)))

  def fieldOrderingOption(viewName: String): Option[Ordering[String]] = viewNameToFieldOrdering.get(viewName)
  def fieldOrdering(viewName: String): Ordering[String] = fieldOrderingOption(viewName)
    .getOrElse(sys.error(s"Field ordering for view $viewName not found"))
  def fieldOrderingOption[T <: AnyRef: Manifest]: Option[Ordering[String]] = fieldOrderingOption(viewName[T])
  def fieldOrdering[T <: AnyRef](implicit mf: Manifest[T]): Ordering[String] = fieldOrderingOption[T]
    .getOrElse(sys.error(s"Field ordering for view $mf not found"))
  def viewDefOption(viewName: String): Option[ViewDef] = viewDefs.get(viewName)
  def viewDef(viewName: String) = viewDefOption(viewName)
    .getOrElse(sys.error(s"View definition for $viewName not found"))
  def viewDefOption[T <: AnyRef: Manifest]: Option[ViewDef] = viewDefOption(viewName[T])
  def viewDef[T <: AnyRef](implicit mf: Manifest[T]): ViewDef =
    viewDefOption[T].getOrElse(sys.error(s"View definition for type $mf not found"))

  def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
    Naming.dasherize(mf.runtimeClass.getSimpleName).replace("-", "_")
}
