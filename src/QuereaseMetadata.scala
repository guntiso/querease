package querease

import org.tresql.compiling.CompilerFunctionMetadata
import org.tresql.compiling.TresqlFunctionSignatures
import mojoz.metadata._
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.TableDef.TableDefBase
import mojoz.metadata.ColumnDef.ColumnDefBase
import mojoz.metadata.in.{YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}
import scala.collection.immutable.Seq

trait QuereaseMetadata {

  type FieldDef = mojoz.metadata.FieldDef[Type]
  type ViewDef = mojoz.metadata.ViewDef[FieldDef]

  class FieldOrdering(val nameToIndex: Map[String, Int]) extends Ordering[String] {
    override def compare(x: String, y: String) =
      nameToIndex.getOrElse(x, 999) - nameToIndex.getOrElse(y, 999)
  }
  object FieldOrdering {
    def apply(view: ViewDef) =
      new FieldOrdering(view.fields.map(f => Option(f.alias) getOrElse f.name).zipWithIndex.toMap)
  }

  protected lazy val yamlMetadata = YamlMd.fromResources()
  lazy val metadataConventions: MdConventions = new SimplePatternMdConventions
  lazy val typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs
  lazy val tableMetadata: TableMetadata[TableDefBase[ColumnDefBase[Type]]] =
    new TableMetadata(new YamlTableDefLoader(yamlMetadata, metadataConventions, typeDefs).tableDefs)
  lazy val functionSignaturesClass: Class[_] = classOf[TresqlFunctionSignatures]
  lazy val tresqlMetadata = new TresqlMetadata(tableMetadata.tableDefs, null, typeDefs) with CompilerFunctionMetadata {
    override def compilerFunctionSignatures = functionSignaturesClass
  }
  protected lazy val tresqlJoinsParser = new TresqlJoinsParser(tresqlMetadata)

  lazy val viewDefs: Map[String, ViewDef] =
    YamlViewDefLoader(tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions, Nil, typeDefs)
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
    mf.runtimeClass.getSimpleName
}
