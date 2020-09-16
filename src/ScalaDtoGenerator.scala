package org.mojoz.querease

import org.mojoz.metadata.out.ScalaGenerator
import org.mojoz.metadata.MojozFieldDefBase
import org.mojoz.metadata.MojozViewDefBase
import org.mojoz.metadata.Type

import org.tresql.parsing.Ident
import org.tresql.parsing.Variable

import scala.annotation.tailrec
import scala.collection.immutable.Seq

/** Generates scala code and adds resolver methods for convenience */
class ScalaDtoGenerator(qe: Querease) extends ScalaGenerator(qe.typeDefs) {
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val SimpleIdentR = ("^" + ident + "$").r
  private val q3 = "\"\"\"" // https://github.com/scala/bug/issues/6476
  override def scalaClassTraits(viewDef: MojozViewDefBase): Seq[String] =
    if (viewDef.fields.exists(f => Option(f.alias).getOrElse(f.name) == "id" && f.type_.name == "long"))
      List("DtoWithId")
    else List("Dto")
  def useTresqlInterpolator = true
  def transformResolverExpression(expression: String, field: MojozFieldDefBase): String = {
    import qe.parser._
    transformer {
      case Ident(List("_")) if field != null =>
        Variable(Option(field.alias).getOrElse(field.name), Nil, false)
    } (parseExp(expression)).tresql
  }
  private def resolverDefs(
      viewDef: MojozViewDefBase,
      qe: Querease with QuereaseResolvers,
      manageOverrides: Boolean,
      shouldGenerateResolverDef: (MojozViewDefBase, MojozFieldDefBase, String) => Boolean,
      generateResolverDef: (MojozViewDefBase, MojozFieldDefBase, Boolean, String) => String
  ): Seq[String] = {
    def resolverExp(viewDef: MojozViewDefBase, f: MojozFieldDefBase) =
      qe.allResolvers(
        viewDef.asInstanceOf[this.qe.ViewDef],
        f.asInstanceOf[this.qe.FieldDef]
      ).map { exp =>
        transformResolverExpression(exp, f)
      }.collect {
        case exp if shouldGenerateResolverDef(viewDef, f, exp) => exp
      }
    def resolvers(viewDef: MojozViewDefBase, fields: Seq[MojozFieldDefBase], manageOverrides: Boolean): Seq[String] =
      fields.flatMap { f => resolverExp(viewDef, f).map { resolverExpression =>
        val isOverride =
          if (manageOverrides && f.isOverride) {
            val fieldName = Option(f.alias).getOrElse(f.name)
            // FIXME isOverride detection: match resolver parameter signatures!
            @tailrec
            def hasSuperResolver(viewDef: MojozViewDefBase): Boolean = {
              val fSuperOpt = viewDef.fields.find(f => Option(f.alias).getOrElse(f.name) == fieldName)
              if (fSuperOpt.nonEmpty && resolvers(viewDef, fSuperOpt.toList, manageOverrides = false).nonEmpty)
                true
              else if (viewDef.extends_ != null)
                hasSuperResolver(qe.viewDef(viewDef.extends_))
              else false
            }
            hasSuperResolver(qe.viewDef(viewDef.extends_))
          } else false
        generateResolverDef(viewDef, f, isOverride, resolverExpression)
      }}
      .filterNot(_ == null)
      .filterNot(_ == "")
      .toSeq
    resolvers(viewDef, viewDef.fields, manageOverrides)
      .map(_ + nl)
  }
  def shouldGenerateInstanceResolverDefs: Boolean = true
  def instanceResolverDefs(
      viewDef: MojozViewDefBase,
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    resolverDefs(viewDef, qe, manageOverrides = true, shouldGenerateInstanceResolverDef, instanceResolverDef)
  }
  def shouldGenerateCompanionResolverDefs: Boolean = true
  def companionResolverDefs(
      viewDef: MojozViewDefBase,
      qe: Querease with QuereaseResolvers
  ): Seq[String] = {
    resolverDefs(viewDef, qe, manageOverrides = false, shouldGenerateCompanionResolverDef, companionResolverDef)
  }
  def resolverDefBodyPrefix: String = ""
  def shouldGenerateInstanceResolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      resolverExpression: String
  ): Boolean = {
    resolverExpression != null && resolverExpression != ""
  }
  def shouldGenerateCompanionResolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
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
  private def isFieldDefined(viewDef: MojozViewDefBase, path: String): Boolean =
    qe.findField(viewDef, path).nonEmpty
  private def getParamNames(expression: String): Seq[String] = {
    val variables = qe.parser.extractVariables(expression)
    val optionalVarsSet =
      variables.filter(_.opt).map(_.copy(opt = false).tresql.replaceAll("^:", "")).toSet
    variables.map(v => (if (v.opt) v.copy(opt = false) else v).tresql.replaceAll("^:", ""))
      .distinct
      .map { case v if optionalVarsSet.contains(v) => v + "?" case v => v }
  }
  def instanceResolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      isOverride: Boolean,
      resolverExpression: String
  ): String = {
    val xViewDef = qe.nameToViewDef(viewDef.name)
    val paramNames =
      getParamNames(resolverExpression)
        .filterNot(n =>
          isFieldDefined(xViewDef, if ((n endsWith "?") && (n != "?")) n dropRight 1 else n))
    val defaultParamsString = "this.toMap"
    resolverDef(viewDef, fieldDef, isOverride, paramNames, defaultParamsString, resolverExpression)
  }
  def companionResolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      isOverride: Boolean,
      resolverExpression: String
  ): String = {
    val paramNames = getParamNames(resolverExpression)
    val defaultParamsString = ""
    resolverDef(viewDef, fieldDef, isOverride, paramNames, defaultParamsString, resolverExpression)
  }
  def resolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      isOverride: Boolean,
      paramNames: Seq[String],
      defaultParamsString: String,
      resolverExpression: String
  ): String = {
    val xViewDef = qe.nameToViewDef(viewDef.name)
    val hasDefaultParams = defaultParamsString != null && defaultParamsString != ""
    val optionalParamsSet =
      Option(paramNames).getOrElse(Nil)
        .filter(_ endsWith "?").filterNot(_ == "?").map(_ dropRight 1).toSet
    val paramNamesReduced =
      Option(paramNames)
        .getOrElse(Nil)
        .map(n => if ((n endsWith "?") && (n != "?")) n dropRight 1 else n)
        .map { name =>
          if (SimpleIdentR.pattern.matcher(name).matches)
            name
          else if (isFieldDefined(xViewDef, name))
            qe.parser.extractVariables(":" + name).map(_.variable).head
          else {
            val nameParts =
              qe.parser.extractVariables(if (name == "?") name else ":" + name)
                .flatMap(v => v.variable :: v.members)
            if (nameParts.size == 1 && nameParts(0) != "?")
                 nameParts(0)
            else null
          }
        }.zipWithIndex.reverse.toMap.toList.sortBy(_._2).map(_._1) // distinct (with null) for scala 2.10
    val hasKnownParams = paramNamesReduced.filter(_ != null).nonEmpty
    val notFoundInChild = paramNamesReduced.exists(_ == null)
    val hasOptionalParams = paramNamesReduced.exists(optionalParamsSet.contains)
    val NotFoundParamName = "`[APPLY OTHER]`"
    def varName(p: String) =
      if (SimpleIdentR.pattern.matcher(p).matches) p
      else Variable(p, Nil, false).tresql.replaceAll("^:", "")
    val paramsKeyValueScalaString =
      paramNamesReduced.filter(_ != null).filterNot(optionalParamsSet.contains).map { p =>
        val paramType = resolverParamType(viewDef, varName(p))
        if (paramType.isComplexType)
             s""""$p" -> Option(${scalaNameString(p)}).map(_.toMap).orNull"""
        else s""""$p" -> ${scalaNameString(p)}"""
      }.mkString(", ")
    val optionalParamsKeyValueScalaString =
      paramNamesReduced.filter(_ != null).filter(optionalParamsSet.contains).map { p =>
        val paramType = resolverParamType(viewDef, varName(p))
        val value = if (paramType.isComplexType) "Option(v).map(_.toMap).orNull" else "v"
        s"""Option(${scalaNameString(p)}).filter(_.nonEmpty).map(_.get).map(v => "$p" -> $value)"""
      }.mkString(", ")
    val argsString =
      if (paramNamesReduced.nonEmpty)
        paramNamesReduced.map { p =>
          if (p == null)
            s"$NotFoundParamName: Map[String, Any] => Map[String, Any]"
          else if (optionalParamsSet contains p)
            s"""${scalaNameString(p)}: Option[${resolverParamTypeName(viewDef, varName(p))}]"""
          else
            s"""${scalaNameString(p)}: ${resolverParamTypeName(viewDef, varName(p))}"""
        }.mkString("(", ", ", ")")
      else ""
    val paramsStringKnown =
      List(!(hasDefaultParams || hasKnownParams || hasOptionalParams) -> "Map.empty",
           hasDefaultParams         ->    defaultParamsString,
           hasKnownParams           ->  s"Map($paramsKeyValueScalaString)",
           hasOptionalParams        ->  s"List($optionalParamsKeyValueScalaString).filter(_.nonEmpty).map(_.get).toMap"
      ).filter(_._1).map(_._2).mkString(" ++ ")
    val paramsString =
      if (notFoundInChild) s"$NotFoundParamName($paramsStringKnown)" else paramsStringKnown
    val override_ = if (isOverride) "override " else ""
    s"  ${override_}def resolve_${resolverTargetColName(fieldDef)}$argsString = $resolverDefBodyPrefix{" + nl +
          resolverBody(resolverExpression, paramsString, resolverTargetTypeName(fieldDef)) +
    s"  }"
  }
  override def scalaObjectString(viewDef: MojozViewDefBase): String = {
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

  def resolverTargetColName(f: MojozFieldDefBase): String =
    Option(f.saveTo) getOrElse f.name
  def resolverTargetType(f: MojozFieldDefBase): Type = {
    val colName = resolverTargetColName(f)
    Option(f.table)
      .flatMap(qe.tableMetadata.tableDefOption)
      .flatMap(_.cols.find(_.name == colName))
      .map(_.type_)
      .orElse(Option(qe.metadataConventions.typeFromExternal(colName, None)))
      .filterNot(_.name == null)
      .getOrElse(new Type("string"))
  }
  def resolverTargetTypeName(f: MojozFieldDefBase): String =
    scalaSimpleTypeName(resolverTargetType(f))
  def resolverParamType(viewDef: MojozViewDefBase, paramName: String): Type =
    qe.findField(viewDef, paramName)
      .map(_.type_)
      .orElse(Option(qe.metadataConventions.typeFromExternal(paramName, None)))
      .filterNot(_.name == null)
      .getOrElse(new Type("string"))
  def resolverParamTypeName(viewDef: MojozViewDefBase, paramName: String): String =
    scalaTypeName(resolverParamType(viewDef, paramName))

  override def scalaBodyExtra(viewDef: MojozViewDefBase): String = qe match {
    case resolvers: QuereaseResolvers if shouldGenerateInstanceResolverDefs =>
      instanceResolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
