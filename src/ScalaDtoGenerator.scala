package querease

import mojoz.metadata._
import mojoz.metadata.out.ScalaClassWriter

import scala.collection.immutable.Seq

class ScalaDtoGenerator(qe: Querease) extends ScalaClassWriter(qe.typeDefs) {
  override def scalaClassTraits(viewDef: ViewDef.ViewDefBase[FieldDef.FieldDefBase[Type]]): Seq[String] =
    if (viewDef.fields.exists(f => Option(f.alias).getOrElse(f.name) == "id" && f.type_.name == "long"))
      List("DtoWithId")
    else List("Dto")
}
