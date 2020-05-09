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
        ).map((Option(f.saveTo) getOrElse f.name, _))
      }
      .filterNot(_._2.indexOf("^") >= 0) // XXX will not compile if not converted to tresql
      .map { r =>
        s"  def resolve_${r._1} = dbUse {" + nl +
        s"    tresql$q3({${r._2}})$q3(Env.withParams(this.toMap))" + nl +
        s"      .unique[${if (r._1 == "id" || r._1.endsWith("_id")) "java.lang.Long" else "String"}]" + nl + // FIXME find type
        s"  }" + nl
      }
  }

  override def scalaBodyExtra(viewDef: ViewDefBase[FieldDefBase[Type]]): String = qe match {
    case resolvers: QuereaseResolvers =>
      resolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
