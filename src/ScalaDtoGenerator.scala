package querease

import mojoz.metadata.out.ScalaClassWriter
import mojoz.metadata.FieldDef.FieldDefBase
import mojoz.metadata.ViewDef.ViewDefBase
import mojoz.metadata.Type

import org.tresql.parsing.Ident
import org.tresql.parsing.Variable

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
  def useTresqlInterpolator = true
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
  def shouldGenerateInstanceResolverDefs: Boolean = true
  def instanceResolverDefs(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    resolverDefs(viewDef, qe, shouldGenerateInstanceResolverDef, instanceResolverDef)
  }
  def shouldGenerateCompanionResolverDefs: Boolean = true
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
    resolverExpression != null && resolverExpression != ""
  }
  def resolverBody(
      resolverExpression: String,
      resolverParams: String,
      resolverTargetType: String
  ): String = {
    val resources = resourcesWithParams(resolverParams)
    if (useTresqlInterpolator)
      s"    tresql$q3{$resolverExpression}$q3($resources)" + nl +
      s"      .unique[$resolverTargetType]" + nl
    else
      s"    Query($q3{$resolverExpression}$q3)($resources)" + nl +
      s"      .unique[$resolverTargetType]" + nl
  }
  def resourcesWithParams(params: String): String =
    s"Env.withParams($params)"
  private def isFieldDefined(viewDef: ViewDefBase[FieldDefBase[Type]], name: String): Boolean = {
    def findField(viewDef: ViewDefBase[FieldDefBase[Type]], name: String) =
      Option(viewDef)
        .map(_.fields)
        .filter(_ != null)
        .flatMap(_.find(f => Option(f.alias).getOrElse(f.name) == name))
    if (SimpleIdentR.pattern.matcher(name).matches)
      findField(viewDef, name).map(_ => true) getOrElse false
    else {
      val nameParts = name.split("""\.""")
      val descViewDef =
        nameParts.dropRight(1).foldLeft(Option(viewDef)) {
          case (vOpt, n) =>
            vOpt flatMap { v =>
              findField(v, n)
                .filter(_.type_.isComplexType)
                .flatMap(_ => qe.viewDefOption(n))
            }
        }.getOrElse(null)
      findField(descViewDef, nameParts.last).map(_ => true) getOrElse false
    }
  }
  def instanceResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): String = {
    val variables = qe.parser.extractVariables(resolverExpression)
    val paramNames: Seq[String] =
      variables.map(_.variable).zipWithIndex.reverse.toMap.toList.sortBy(_._2).map(_._1)
        .filterNot(isFieldDefined(viewDef, _))
    val defaultParamsString = "this.toMap"
    resolverDef(viewDef, fieldDef, paramNames, defaultParamsString, resolverExpression)
  }
  def companionResolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      resolverExpression: String
  ): String = {
    val variables = qe.parser.extractVariables(resolverExpression)
    val paramNames: Seq[String] =
      variables.map(_.variable).zipWithIndex.reverse.toMap.toList.sortBy(_._2).map(_._1)
    val defaultParamsString = ""
    resolverDef(viewDef, fieldDef, paramNames, defaultParamsString, resolverExpression)
  }
  def resolverDef(
      viewDef: ViewDefBase[FieldDefBase[Type]],
      fieldDef: FieldDefBase[Type],
      paramNames: Seq[String],
      defaultParamsString: String,
      resolverExpression: String
  ): String = {
    val hasParams = paramNames != null && paramNames.nonEmpty
    val hasDefaultParams = defaultParamsString != null && defaultParamsString != ""
    val paramNamesReduced =
      Option(paramNames)
        .getOrElse(Nil)
        .map { name =>
          if (SimpleIdentR.pattern.matcher(name).matches)
            name
          else if (isFieldDefined(viewDef, name))
            name.split("""\.""").head
          else
            name
        }.zipWithIndex.reverse.toMap.toList.sortBy(_._2).map(_._1)
    val paramsKeyValueScalaString =
      paramNamesReduced.map { p =>
        val paramType = resolverParamType(viewDef, p)
        if (paramType.isComplexType)
             s""""$p" -> Option(${scalaNameString(p)}).map(_.toMap).orNull"""
        else s""""$p" -> ${scalaNameString(p)}"""
      }.mkString(", ")
    val argsString =
      if (hasParams)
        paramNamesReduced.map(p => s"""${scalaNameString(p)}: ${resolverParamTypeName(viewDef, p)}""")
          .mkString("(", ", ", ")")
      else ""
    val paramsString = (hasParams, hasDefaultParams) match {
      case (false, false) =>  "Map.empty"
      case (false,  true) =>   defaultParamsString
      case (true,  false) => s"Map($paramsKeyValueScalaString)"
      case (true,   true) => s"$defaultParamsString ++ Map($paramsKeyValueScalaString)"
    }
    s"  def resolve_${resolverTargetColName(fieldDef)}$argsString = $resolverDefBodyPrefix{" + nl +
          resolverBody(resolverExpression, paramsString, resolverTargetTypeName(fieldDef)) +
    s"  }"
  }
  def scalaNameString(name: String) = name
  override def createScalaClassString(viewDef: ViewDefBase[FieldDefBase[Type]]) = {
    Seq(super.createScalaClassString(viewDef), scalaObjectString(viewDef))
      .filter(_ != null).filter(_ != "").mkString(nl)
  }
  def scalaObjectString(viewDef: ViewDefBase[FieldDefBase[Type]]): String = {
    val className = scalaClassName(viewDef.name)
    val resolverDefs = (qe match {
      case resolvers: QuereaseResolvers if shouldGenerateCompanionResolverDefs =>
        companionResolverDefs(viewDef, resolvers)
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
  def resolverTargetType(f: FieldDefBase[Type]): Type = {
    val colName = resolverTargetColName(f)
    Option(f.table)
      .flatMap(qe.tableMetadata.tableDefOption)
      .flatMap(_.cols.find(_.name == colName))
      .map(_.type_)
      .getOrElse(qe.metadataConventions.fromExternal(colName, None, None)._1)
  }
  def resolverTargetTypeName(f: FieldDefBase[Type]): String =
    scalaSimpleTypeName(resolverTargetType(f))
  def resolverParamType(viewDef: ViewDefBase[FieldDefBase[Type]], paramName: String): Type = {
    Option(viewDef)
      .map(_.fields)
      .filter(_ != null)
      .flatMap(_.find(f => Option(f.alias).getOrElse(f.name) == paramName))
      .map(_.type_)
      .getOrElse(qe.metadataConventions.fromExternal(paramName, None, None)._1)
  }
  def resolverParamTypeName(viewDef: ViewDefBase[FieldDefBase[Type]], paramName: String): String =
    scalaTypeName(resolverParamType(viewDef, paramName))
  def scalaTypeName(type_ : Type): String =
    if  (type_.isComplexType)
         scalaComplexTypeName(type_)
    else scalaSimpleTypeName(type_)

  override def scalaBodyExtra(viewDef: ViewDefBase[FieldDefBase[Type]]): String = qe match {
    case resolvers: QuereaseResolvers if shouldGenerateInstanceResolverDefs =>
      instanceResolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
