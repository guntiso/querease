package querease

import mojoz.metadata.out.ScalaClassWriter
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.Type

import scala.collection.immutable.Seq

/** Generates scala code and adds resolver methods for convenience */
class ScalaDtoGenerator(qe: Querease) extends ScalaClassWriter(qe.typeDefs) {
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val SimpleIdentR = ("^" + ident + "$").r
  private val q3 = "\"\"\"" // https://github.com/scala/bug/issues/6476
  override def scalaClassTraits(viewDef: ViewDefBase[FieldDefBase[Type]]): Seq[String] =
    if (viewDef.fields.exists(f => Option(f.alias).getOrElse(f.name) == "id" && f.type_.name == "long"))
      List("DtoWithId")
    else List("Dto")

  def transformResolverExpression(expression: String, field: FieldDefBase[Type]): String = {
    import qe.parser._
    transformer {
      case Ident(List("_")) if field != null =>
        Variable(Option(field.alias).getOrElse(field.name), Nil, false)
    } (parseExp(expression)).tresql
  }
  private def resolverDefs(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      qe: Querease with QuereaseResolvers,
      shouldGenerateResolverDef: (ViewDefBase[FieldDefBase[Type]], FieldDefBase[Type], String) => Boolean,
      generateResolverDef: (ViewDefBase[FieldDefBase[Type]], FieldDefBase[Type], String) => String
  ): Seq[String] = {
    viewDef.fields
      .filterNot(_.type_.isComplexType)
      .flatMap { f =>
        qe.allResolvers(
          viewDef.asInstanceOf[this.qe.ViewDef],
          f.asInstanceOf[this.qe.FieldDef]
        ).map { exp =>
          val resolverExpression = transformResolverExpression(exp, f)
          if  (shouldGenerateResolverDef(viewDef, f, resolverExpression))
               generateResolverDef(viewDef, f, resolverExpression)
          else null
        }
      }
      .filterNot(_ == null)
      .filterNot(_ == "")
      .map(_ + nl)
  }
  def instanceResolverDefs(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    resolverDefs(viewDef, qe, shouldGenerateInstanceResolverDef, instanceResolverDef)
  }
  def companionResolverDefs(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    resolverDefs(viewDef, qe, shouldGenerateCompanionResolverDef, companionResolverDef)
  }
  def resolverDefBodyPrefix: String = ""
  def shouldGenerateInstanceResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): Boolean = {
    resolverExpression != null && resolverExpression != ""
  }
  def shouldGenerateCompanionResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): Boolean = {
    resolverExpression != null && resolverExpression != "" &&
      (qe.parser.extractVariables(resolverExpression).map(_.variable).toSet.toList match {
        case Nil => true
        case lst => !lst.exists(!SimpleIdentR.pattern.matcher(_).matches)
      })
  }
  def instanceResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): String = {
    s"  def resolve_${resolverTargetColName(fieldDef)} = $resolverDefBodyPrefix{" + nl +
    s"    tresql$q3{$resolverExpression}$q3(Env.withParams(this.toMap))" + nl +
    s"      .unique[${resolverTargetTypeName(fieldDef)}]" + nl +
    s"  }"
  }
  def companionResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): String = {
    val variables = qe.parser.extractVariables(resolverExpression)
    if (variables.exists(v => !SimpleIdentR.pattern.matcher(v.variable).matches))
      null // TODO?
    else {
      val paramNames: Seq[String] =
        variables.map(_.variable).zipWithIndex.reverse.toMap.toList.sortBy(_._2).map(_._1)
      val paramsKeyValueScalaString =
        paramNames.map(p => s""""$p" -> ${scalaNameString(p)}""").mkString(", ")
      val argsString =
        paramNames.map(p => s"""${scalaNameString(p)}: ${resolverParamTypeName(viewDef, p)}""")
          .mkString(", ")
      s"  def resolve_${resolverTargetColName(fieldDef)}($argsString) = $resolverDefBodyPrefix{" + nl +
      s"    tresql$q3{$resolverExpression}$q3(Env.withParams(Map($paramsKeyValueScalaString)))" + nl +
      s"      .unique[${resolverTargetTypeName(fieldDef)}]" + nl +
      s"  }"
    }
  }
  def scalaNameString(name: String) = name
  override def createScalaClassString(viewDef: ViewDefBase[FieldDefBase[Type]]) = {
    Seq(super.createScalaClassString(viewDef), scalaObjectString(viewDef))
      .filter(_ != null).filter(_ != "").mkString(nl)
  }
  def scalaObjectString(viewDef: ViewDefBase[FieldDefBase[Type]]): String = {
    val className = scalaClassName(viewDef.name)
    val resolverDefs = (qe match {
      case resolvers: QuereaseResolvers => companionResolverDefs(viewDef, resolvers)
      case _ => Nil
    })
    if (resolverDefs.nonEmpty) {
      s"object $className {" + nl +
        resolverDefs.mkString +
      s"}"
    } else null
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
  def resolverParamTypeName(viewDef: ViewDefBase[FieldDefBase[Type]], paramName: String): String = {
    val type_ = Option(viewDef)
      .map(_.fields)
      .filter(_ != null)
      .flatMap(_.find(f => Option(f.name).getOrElse(f.alias) == paramName))
      .map(_.type_)
      .getOrElse(qe.metadataConventions.fromExternal(paramName, None, None)._1)
    scalaSimpleTypeName(type_)
  }

  override def scalaBodyExtra(viewDef: ViewDefBase[FieldDefBase[Type]]): String = qe match {
    case resolvers: QuereaseResolvers =>
      instanceResolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
