package org.mojoz.querease

import org.tresql.compiling.TresqlFunctionSignatures
import org.mojoz.metadata._
import org.mojoz.metadata.TableDef.TableDefBase
import org.mojoz.metadata.ColumnDef.ColumnDefBase
import org.mojoz.metadata.in.{YamlMd, YamlTableDefLoader, YamlViewDefLoader}
import org.mojoz.metadata.io.{MdConventions, SimplePatternMdConventions}

import scala.collection.immutable.Seq
import scala.util.matching.Regex

case class ViewNotFoundException(message: String) extends Exception(message)
case class FieldOrderingNotFoundException(message: String) extends Exception(message)

trait QuereaseMetadata {

  type FieldDef = org.mojoz.metadata.FieldDef[Type]
  type ViewDef = org.mojoz.metadata.ViewDef[FieldDef]

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

  lazy val viewDefLoader: YamlViewDefLoader =
    YamlViewDefLoader(tableMetadata, yamlMetadata, tresqlJoinsParser, metadataConventions, Nil, typeDefs)
  import QuereaseMetadata.toQuereaseViewDefs
  lazy val nameToViewDef: Map[String, ViewDef] = toQuereaseViewDefs {
    viewDefLoader
      .nameToViewDef.asInstanceOf[Map[String, ViewDef]]
  }
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

object QuereaseMetadata {
  trait QuereaseViewDefExtras {
    val validations: Seq[String]
  }

  private [querease] case class QuereaseViewDef(
    validations: Seq[String] = Nil
  ) extends QuereaseViewDefExtras

  trait QuereaseFieldDefExtras {
    val initial: String
  }

  private [querease] case class QuereaseFieldDef(
    initial: String = null
  ) extends QuereaseFieldDefExtras

  val BindVarCursorsCmd = "build cursors"
  val BindVarCursorsCmdRegex =
    new Regex(BindVarCursorsCmd + """((\s+:\w+)*)""")
  val BindVarCursorsForViewCmd = "build cursors for view"
  val BindVarCursorsForViewCmdRegex =
    new Regex(BindVarCursorsForViewCmd + """\s+(:?\w+)((\s+:\w+)*)""")

  val QuereaseViewExtrasKey = "querease-view-extras"
  val QuereaseFieldExtrasKey = "querease-field-extras"
  trait ExtrasMap {
    protected def updateExtrasMap(extras: Map[String, Any]): Any
    protected def extrasMap: Map[String, Any]

    private def extrasMapOrEmpty: Map[String, Any] = Option(extrasMap).getOrElse(Map())
    protected def updateExtras[T, E](key: String, updater: E => E, default: E): T =
      updateExtrasMap(extrasMapOrEmpty + (key -> updater(extras(key, default)))).asInstanceOf[T]
    protected def extras[E](key: String, default: E): E = extrasMapOrEmpty.getOrElse(key, default).asInstanceOf[E]
  }
  implicit class AugmentedQuereaseViewDef(viewDef: QuereaseMetadata#ViewDef) extends QuereaseViewDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseViewDef()
    private val quereaseExtras = extras(QuereaseViewExtrasKey, defaultExtras)
    override val validations = quereaseExtras.validations
    def updateExtras(updater: QuereaseViewDef => QuereaseViewDef): QuereaseMetadata#ViewDef =
      updateExtras(QuereaseViewExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]) = viewDef.copy(extras = extras)
    override protected def extrasMap = viewDef.extras
  }
  implicit class AugmentedQuereaseFieldDef(fieldDef: QuereaseMetadata#FieldDef) extends QuereaseFieldDefExtras with ExtrasMap {
    private val defaultExtras = QuereaseFieldDef()
    private val quereaseExtras = extras(QuereaseFieldExtrasKey, defaultExtras)
    override val initial = quereaseExtras.initial
    def updateExtras(updater: QuereaseFieldDef => QuereaseFieldDef): QuereaseMetadata#FieldDef =
      updateExtras(QuereaseFieldExtrasKey, updater, defaultExtras)

    override protected def updateExtrasMap(extras: Map[String, Any]): Any = fieldDef.copy(extras = extras)
    override protected def extrasMap = fieldDef.extras
  }

  def toQuereaseViewDefs(mojozViewDefs: Map[String, MojozViewDef]): Map[String, MojozViewDef] =
    mojozViewDefs.map(kv => kv._1 -> toQuereaseViewDef(kv._2)).toMap

  def toQuereaseViewDef(viewDef: MojozViewDef): MojozViewDef = {
    import scala.jdk.CollectionConverters._
    val Initial = "initial"
    val Validations = "validations"
    def getExtraOpt(f: MojozFieldDef, key: String) =
      Option(f.extras).flatMap(_ get key).map {
        case s: String => s
        case i: Int => i.toString
        case l: Long => l.toString
        case d: Double => d.toString
        case bd: BigDecimal => bd.toString
        case b: Boolean => b.toString
        case null => null
        case x => sys.error(
          s"Expecting String, AnyVal, BigDecimal value or no value, viewDef field, key: ${viewDef.name}.${f.name}, $key")
    }
    def getStringSeq(name: String, extras: Map[String, Any]): Seq[String] = {
      getSeq(name, extras) map {
        case s: java.lang.String => s
        case m: java.util.Map[_, _] =>
          if (m.size == 1) m.entrySet.asScala.toList(0).getKey.toString
          else m.toString // TODO error?
        case x => x.toString
      }
    }
    def getSeq(name: String, extras: Map[String, Any]): Seq[_] =
      Option(extras).flatMap(_ get name) match {
        case Some(s: java.lang.String) => Seq(s)
        case Some(a: java.util.ArrayList[_]) => a.asScala.toList
        case None => Nil
        case Some(null) => Seq("")
        case Some(x) => Seq(x)
      }
    val qeFields = viewDef.fields map { f =>
      val initial = getExtraOpt(f, Initial).orNull
      f.updateExtras(_ => QuereaseFieldDef(initial))
    }
    val validations = getStringSeq(Validations, viewDef.extras)
    viewDef.copy(fields = qeFields).updateExtras(_ => QuereaseViewDef(validations))
  }
}
