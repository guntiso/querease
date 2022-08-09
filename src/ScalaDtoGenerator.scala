package org.mojoz.querease

import org.mojoz.metadata.out.ScalaGenerator
import org.mojoz.metadata.MojozFieldDefBase
import org.mojoz.metadata.MojozViewDefBase
import org.mojoz.metadata.Type

import org.tresql.parsing.Ident
import org.tresql.parsing.Variable

import scala.annotation.tailrec
import scala.collection.immutable.Seq

object ScalaDtoGenerator {
  /** exposes parameter types for override management */
  case class ResolverScala(
    parameterTypes: Seq[String],
    resolver: String
  )
}

/** Generates scala code and adds resolver methods for convenience */
class ScalaDtoGenerator(qe: Querease) extends ScalaGenerator(qe.typeDefs) {
  import ScalaDtoGenerator.ResolverScala
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val SimpleIdentR = ("^" + ident + "$").r
  private val q3 = "\"\"\"" // https://github.com/scala/bug/issues/6476
  override def scalaClassTraits(viewDef: MojozViewDefBase): Seq[String] =
    if (qe.viewDefOption(viewDef.name).getOrElse(viewDef)
          .fieldOpt("id").exists(f => qe.supportedIdTypeNames.headOption.contains(f.type_.name)))
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
      generateResolver: (MojozViewDefBase, MojozFieldDefBase, Boolean, String) => ResolverScala
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
    def resolvers(viewDef: MojozViewDefBase, fields: Seq[MojozFieldDefBase], manageOverrides: Boolean): Seq[ResolverScala] =
      fields.flatMap { f => resolverExp(viewDef, f).map { resolverExpression =>
        val isOverride =
          if (manageOverrides && f.isOverride) {
            val fieldName = Option(f.alias).getOrElse(f.name)
            val pSearch =
              Option(generateResolver(viewDef, f, false, resolverExpression))
                .filterNot(_.resolver == null)
                .filterNot(_.resolver == "")
                .map(_.parameterTypes)
                .orNull
            @tailrec
            def hasMatchingSuperResolver(viewDef: MojozViewDefBase): Boolean = {
              val fSuperOpt = viewDef.fieldOpt(fieldName)
              if (fSuperOpt.nonEmpty &&
                  resolvers(viewDef, fSuperOpt.toList, manageOverrides = false)
                    .exists(_.parameterTypes == pSearch))
                true
              else if (viewDef.extends_ != null)
                hasMatchingSuperResolver(qe.viewDef(viewDef.extends_))
              else false
            }
            pSearch != null && hasMatchingSuperResolver(qe.viewDef(viewDef.extends_))
          } else false
        generateResolver(viewDef, f, isOverride, resolverExpression)
      }}
      .filterNot(_ == null)
      .filterNot(_.resolver == null)
      .filterNot(_.resolver == "")
      .toSeq
    resolvers(viewDef, viewDef.fields, manageOverrides)
      .map(_.resolver + nl)
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
    val escapedResolverExpression =
      resolverExpression
        .replace("""\""", """\\""")
        .replace("""$""", """$$""")
    if (useTresqlInterpolator)
      s"    tresql$q3{$escapedResolverExpression}$q3($resources)" + nl +
      s"      .unique[$resolverTargetType]" + nl
    else
      s"    Query($q3{$escapedResolverExpression}$q3)($resources)" + nl +
      s"      .unique[$resolverTargetType]" + nl
  }
  def instanceResolverDefExtraParams: String =
    s"(implicit env: org.tresql.Resources, qe: QE)"
  def companionResolverDefExtraParams: String =
    s"(implicit env: org.tresql.Resources)"
  def resourcesWithParams(params: String): String =
    s"env.withParams($params)"
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
  ): ResolverScala = {
    val xViewDef = qe.nameToViewDef(viewDef.name)
    val paramNames =
      getParamNames(resolverExpression)
        .filterNot(n =>
          isFieldDefined(xViewDef, if ((n endsWith "?") && (n != "?")) n dropRight 1 else n))
    val defaultArgsString = "this.toMap"
    resolverDef(viewDef, fieldDef, isOverride, paramNames,
      defaultArgsString, instanceResolverDefExtraParams, resolverExpression)
  }
  def companionResolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      isOverride: Boolean,
      resolverExpression: String
  ): ResolverScala = {
    val paramNames = getParamNames(resolverExpression)
    val defaultArgsString = ""
    resolverDef(viewDef, fieldDef, isOverride, paramNames,
      defaultArgsString, companionResolverDefExtraParams, resolverExpression)
  }
  def resolverDef(
      viewDef: MojozViewDefBase,
      fieldDef: MojozFieldDefBase,
      isOverride: Boolean,
      paramNames: Seq[String],
      defaultArgsString: String,
      extraParams: String,
      resolverExpression: String
  ): ResolverScala = {
    val xViewDef = qe.nameToViewDef(viewDef.name)
    val hasDefaultArgs = defaultArgsString != null && defaultArgsString != ""
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
    val paramNamesAndTypes =
      paramNamesReduced.map { p =>
        if (p == null)
          NotFoundParamName -> "Map[String, Any] => Map[String, Any]"
        else if (optionalParamsSet contains p)
          scalaNameString(p) -> s"Option[${resolverParamTypeName(viewDef, varName(p))}]"
        else
          scalaNameString(p) -> resolverParamTypeName(viewDef, varName(p))
      }
    val parameterTypes = paramNamesAndTypes.map(_._2)
    val resolverParams =
      if (paramNamesAndTypes.nonEmpty)
        paramNamesAndTypes.map {
          case (pName, pType) => s"$pName: $pType"
        }.mkString("(", ", ", ")")
      else ""
    val paramsString = resolverParams + extraParams
    val argsStringKnown =
      List(!(hasDefaultArgs || hasKnownParams || hasOptionalParams) -> "Map.empty",
           hasDefaultArgs         ->    defaultArgsString,
           hasKnownParams           ->  s"Map($paramsKeyValueScalaString)",
           hasOptionalParams        ->  s"List($optionalParamsKeyValueScalaString).filter(_.nonEmpty).map(_.get).toMap"
      ).filter(_._1).map(_._2).mkString(" ++ ")
    val argsString =
      if (notFoundInChild) s"$NotFoundParamName($argsStringKnown)" else argsStringKnown
    val override_ = if (isOverride) "override " else ""
    ResolverScala(
      parameterTypes,
      s"  ${override_}def resolve_${resolverTargetColName(fieldDef)}$paramsString = $resolverDefBodyPrefix{" + nl +
            resolverBody(resolverExpression, argsString, resolverTargetTypeName(fieldDef, viewDef.db)) +
      s"  }"
    )
  }
  override def scalaObjectString(viewDef: MojozViewDefBase, allViewDefs: Map[String, MojozViewDefBase]): String = {
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
  def resolverTargetType(f: MojozFieldDefBase, db: String): Type = {
    val colName = resolverTargetColName(f)
    Option(f.table)
      .flatMap(qe.tableMetadata.tableDefOption(_, db))
      .flatMap(_.cols.find(_.name == colName))
      .map(_.type_)
      .orElse(Option(qe.metadataConventions.typeFromExternal(colName, None)))
      .filterNot(_.name == null)
      .getOrElse(new Type("string"))
  }
  def resolverTargetTypeName(f: MojozFieldDefBase, db: String): String =
    scalaSimpleTypeName(resolverTargetType(f, db))
  def resolverParamType(viewDef: MojozViewDefBase, paramName: String): Type =
    qe.findField(viewDef, paramName)
      .map(_.type_)
      .orElse(Option(qe.metadataConventions.typeFromExternal(paramName, None)))
      .filterNot(_.name == null)
      .getOrElse(new Type("string"))
  def resolverParamTypeName(viewDef: MojozViewDefBase, paramName: String): String =
    scalaTypeName(resolverParamType(viewDef, paramName))

  override def scalaBodyExtra(viewDef: MojozViewDefBase, allViewDefs: Map[String, MojozViewDefBase]): String = qe match {
    case resolvers: QuereaseResolvers if shouldGenerateInstanceResolverDefs =>
      instanceResolverDefs(viewDef, resolvers).mkString
    case _ => ""
  }
}
