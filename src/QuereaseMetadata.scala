package querease

import org.tresql.compiling.CompilerFunctionMetadata
import org.tresql.compiling.TresqlFunctionSignatures
import mojoz.metadata._
import mojoz.metadata.TableDef.TableDefBase
import mojoz.metadata.ColumnDef.ColumnDefBase
import mojoz.metadata.in.{YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}
import scala.collection.immutable.Seq

case class ViewNotFoundException(message: String) extends Exception(message)
case class FieldOrderingNotFoundException(message: String) extends Exception(message)

trait QuereaseMetadata {

  type FieldDef = mojoz.metadata.FieldDef[Type]
  type ViewDef = mojoz.metadata.ViewDef[FieldDef]

  class FieldOrdering(val nameToIndex: Map[String, Int]) extends Ordering[String] {
    override def compare(x: String, y: String) =
      nameToIndex.getOrElse(x, 999) - nameToIndex.getOrElse(y, 999) match {
        case 0 => x compareTo y
        case d => d
      }
    override def toString: String =
      s"FieldOrdering(${nameToIndex.toList.sortBy(_._2).map(_._1).mkString(", ")})"
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
  lazy val tresqlMetadata = TresqlMetadata(tableMetadata.tableDefs, typeDefs, functionSignaturesClass)
  protected lazy val tresqlJoinsParser = new TresqlJoinsParser(tresqlMetadata)

  lazy val nameToViewDef: Map[String, ViewDef] =
    YamlViewDefLoader(tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions, Nil, typeDefs)
      .extendedViewDefs.asInstanceOf[Map[String, ViewDef]]
  protected lazy val viewNameToFieldOrdering = nameToViewDef.map(kv => (kv._1, FieldOrdering(kv._2)))

  def fieldOrderingOption(viewName: String): Option[Ordering[String]] = viewNameToFieldOrdering.get(viewName)
  def fieldOrdering(viewName: String): Ordering[String] = fieldOrderingOption(viewName)
    .getOrElse(throw FieldOrderingNotFoundException(s"Field ordering for view $viewName not found"))
  def fieldOrderingOption[T <: AnyRef: Manifest]: Option[Ordering[String]] = fieldOrderingOption(viewName[T])
  def fieldOrdering[T <: AnyRef](implicit mf: Manifest[T]): Ordering[String] = fieldOrderingOption[T]
    .getOrElse(throw FieldOrderingNotFoundException(s"Field ordering for view $mf not found"))
  def viewDefOption(viewName: String): Option[ViewDef] = nameToViewDef.get(viewName)
  def viewDef(viewName: String) = viewDefOption(viewName)
    .getOrElse(throw ViewNotFoundException(s"View definition for $viewName not found"))
  def viewDefOption[T <: AnyRef: Manifest]: Option[ViewDef] = viewDefOption(viewName[T])
  def viewDef[T <: AnyRef](implicit mf: Manifest[T]): ViewDef =
    viewDefOption[T].getOrElse(throw ViewNotFoundException(s"View definition for type $mf not found"))

  def viewName[T <: AnyRef](implicit mf: Manifest[T]): String =
    mf.runtimeClass.getSimpleName
}
