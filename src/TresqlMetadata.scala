package org.mojoz.querease

import java.io.File

import org.tresql.compiling.{CompilerFunctionMetadata, CompilerMetadata, CompilerMetadataFactory}
import org.tresql.Metadata
import org.tresql.metadata.Col
import org.tresql.metadata.Key
import org.tresql.metadata.Procedure
import org.tresql.metadata.{Ref => TresqlRef}
import org.tresql.metadata.Table
import org.tresql.metadata.TypeMapper
import org.mojoz.metadata.io._
import org.mojoz.metadata.in._
import org.mojoz.metadata.Type
import org.mojoz.metadata.TypeDef
import org.mojoz.metadata.TypeMetadata
import org.mojoz.metadata.TableDef.{TableDefBase => TableDef}
import org.mojoz.metadata.ColumnDef.{ColumnDefBase => ColumnDef}

class TresqlMetadata(
  val tableDefs: Seq[TableDef[ColumnDef[Type]]],
  val typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs)
  extends Metadata with TypeMapper {

  val simpleTypeNameToXsdSimpleTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("xsd").orNull)
      .filter(_._2 != null)
      .toMap

  val tables = tableDefs.map { td =>
    def toTresqlCol(c: ColumnDef[Type]) = {
      val typeName = c.type_.name
      val scalaType = xsd_scala_type_map(
        simpleTypeNameToXsdSimpleTypeName.getOrElse(typeName, typeName))
      val jdbcTypeCode = 0 // unknown, not interested
      Col(c.name, c.nullable, jdbcTypeCode, scalaType)
    }
    val name = td.name
    val cols = td.cols.map(toTresqlCol).toList
    val key = Key(td.pk.map(_.cols.toList) getOrElse Nil)
    val refs = td.refs.groupBy(_.refTable)
      .map(kv => kv._1 -> kv._2.map(r => TresqlRef(r.cols.toList, r.refCols.toList)).toList).toMap
    Table(name, cols, key, refs)
  }.map(t => (t.name, t)).toMap

  override def table(name: String) = tables(name)
  override def tableOption(name: String) = tables.get(name)
  override def procedureOption(name: String): Option[Procedure[_]] = None
  lazy val tableMetadataString = {
    def colToString(col: ColumnDef[Type]) =
      col.name +
        (if (!col.nullable) " !" else "") +
        " " + simpleTypeNameToXsdSimpleTypeName.getOrElse(col.type_.name, col.type_.name) // XXX
    def refToString(cols: Seq[String], refTableName: String, refCols: Seq[String]) =
      cols.mkString(", ") + " -> " + refTableName + refCols.mkString("(", ", ", ")")
    import scala.language.implicitConversions
    implicit def stringToSeq(s: String): Seq[String] = Seq(s)
    def tableToString(table: Table, mojozTable: TableDef[ColumnDef[Type]]) =
      Seq[Seq[String]](
        "table: " + table.name,
        "columns:",
        mojozTable.cols.map("- " + colToString(_)),
        if (table.key.cols.size > 0) "pk: " + table.key.cols.mkString(", ") else Nil,
        if (table.rfs.size > 0) "refs:" else Nil,
        table.rfs.toSeq.sortBy(_._1).flatMap(tr =>
          tr._2.map(r => "- " + refToString(r.cols, tr._1, r.refCols)))
      ).flatten.mkString("\n")
    tableDefs.sortBy(_.name).map(t => tableToString(tables(t.name), t)).mkString("\n\n") + "\n"
  }
}


class TresqlMetadataFactory extends CompilerMetadataFactory {
  /** Creates tresql compiler metadata by loading table metadata (as produced by
   *  [[querease.TresqlMetadata.tableMetadataString]]) and instantiating
   *  function signatures class.
   *
   *  Expects "tableMetadataFile" and "functions" keys in conf parameter.
   *  Defaults to "tresql-table-metadata.yaml" and "org.tresql.compiling.TresqlFunctionSignatures"
   */
  override def create(conf: Map[String, String]): CompilerMetadata = {
    val tableMetadataFileName = conf.getOrElse("tableMetadataFile", "tresql-table-metadata.yaml")
    val functionSignaturesClassName =
      conf.getOrElse("functions", "org.tresql.compiling.TresqlFunctionSignatures")
    val macrosClassName = conf.get("macros")
    val rawTableMetadata = YamlMd.fromFile(new File(tableMetadataFileName))
    val mdConventions = new SimplePatternMdConventions(Nil, Nil, Nil)
    val typeDefs = TypeMetadata.defaultTypeDefs // XXX
    val tableDefs = new YamlTableDefLoader(rawTableMetadata, mdConventions, typeDefs).tableDefs
    val functionSignaturesClass = Option(functionSignaturesClassName).map(Class.forName).orNull
    new CompilerMetadata {
      override def metadata: Metadata =
        TresqlMetadata(tableDefs, typeDefs = Nil, functionSignaturesClass)

      override def macros: Option[Any] =
        macrosClassName.map(cn => Class.forName(cn).getDeclaredConstructor().newInstance())
    }
  }
}

object TresqlMetadata {
  /** Creates tresql compiler metadata from table metadata, typedefs and function signatures class.
   */
  def apply(
      tableDefs: Seq[TableDef[ColumnDef[Type]]],
      typeDefs: collection.immutable.Seq[TypeDef],
      functionSignaturesClass: Class[_]): TresqlMetadata = {
    if (functionSignaturesClass == null)
      new TresqlMetadata(tableDefs, typeDefs)
    else {
      new TresqlMetadata(tableDefs, typeDefs) with CompilerFunctionMetadata {
        override def compilerFunctionSignatures = functionSignaturesClass
      }
    }
  }
}
