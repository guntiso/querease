package querease

import mojoz.metadata.out.ScalaClassWriter
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.Type

import scala.collection.immutable.Seq

class ScalaDtoGenerator(qe: Querease) extends ScalaClassWriter(qe.typeDefs) {
  override def scalaClassTraits(viewDef: ViewDefBase[FieldDefBase[Type]]): Seq[String] =
    if (viewDef.fields.exists(f => Option(f.alias).getOrElse(f.name) == "id" && f.type_.name == "long"))
      List("DtoWithId")
    else List("Dto")

  def resolverDefs(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    viewDef.fields
      .filterNot(_.type_.isComplexType)
      .flatMap { f =>
        qe.allResolvers(
          viewDef.asInstanceOf[this.qe.ViewDef],
          f.asInstanceOf[this.qe.FieldDef]
        ).map(exp => resolverDef(viewDef, f, transformResolverExpression(exp, f)))
      }
      .filterNot(_ == null)
      .filterNot(_ == "")
      .map(_ + nl)
  }
  def transformResolverExpression(expression: String, field: FieldDefBase[Type]): String = {
    import qe.parser._
    transformer {
      case Ident(List("_")) if field != null =>
        Variable(Option(field.alias).getOrElse(field.name), Nil, false)
    } (parseExp(expression)).tresql
  }
  def resolverDefBodyPrefix: String = ""
  def resolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): String = {
    val q3 = "\"\"\"" // https://github.com/scala/bug/issues/6476
    s"  def resolve_${resolverTargetColName(fieldDef)} = $resolverDefBodyPrefix{" + nl +
    s"    tresql$q3{$resolverExpression}$q3(Env.withParams(this.toMap))" + nl +
    s"      .unique[${resolverTargetTypeName(fieldDef)}]" + nl +
    s"  }"
  }

  def resolverTargetColName(f: FieldDefBase[Type]): String =
    Option(f.saveTo) getOrElse f.name
  def resolverTargetTypeName(f: FieldDefBase[Type]): String = {
    val colName = resolverTargetColName(f)
    val type_ = Option(f.table)
      .flatMap(qe.tableMetadata.tableDefOption)
      .flatMap(_.cols.find(_.name == colName))
      .map(_.type_)
      .getOrElse(qe.metadataConventions.fromExternal(colName, None, None)._1)
    scalaSimpleTypeName(type_)
  }

  override def scalaBodyExtra(viewDef: ViewDefBase[FieldDefBase[Type]]): String = qe match {
    case resolvers: QuereaseResolvers =>
      resolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
