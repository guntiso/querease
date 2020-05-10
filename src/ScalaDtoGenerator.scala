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
    val q3 = "\"\"\""
    viewDef.fields
      .filterNot(_.type_.isComplexType)
      .flatMap { f =>
        qe.allResolvers(
          viewDef.asInstanceOf[this.qe.ViewDef],
          f.asInstanceOf[this.qe.FieldDef]
        ).map((f, _))
      }
      .filterNot(_._2.indexOf("^") >= 0) // XXX will not compile if not converted to tresql
      .map { case (f, r) =>
        s"  def resolve_${resolverColName(f)} = dbUse {" + nl +
        s"    tresql$q3({${r}})$q3(Env.withParams(this.toMap))" + nl +
        s"      .unique[${resolverTypeName(f)}]" + nl +
        s"  }" + nl
      }
  }

  def resolverColName(f: FieldDefBase[Type]): String =
    Option(f.saveTo) getOrElse f.name
  def resolverTypeName(f: FieldDefBase[Type]): String = {
    val colName = resolverColName(f)
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
