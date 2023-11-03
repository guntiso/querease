package org.mojoz.querease

import org.mojoz.metadata.{ColumnDef, TableDef, TypeDef, TypeMetadata}
import org.mojoz.metadata.in._
import org.mojoz.metadata.io._
import org.mojoz.querease.TresqlMetadata._
import org.tresql.ast.CompilerAst.ExprType
import org.tresql.Metadata
import org.tresql.compiling.{CompilerMetadata, CompilerMetadataFactory}
import org.tresql.metadata.{Col, Key, Table, TypeMapper, Ref => TresqlRef}

import java.io.InputStream
import scala.collection.immutable.{Map, Seq, Set}

class TresqlMetadata(
  val tableDefs: Seq[TableDef],
  val typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs,
  override val macrosClass: Class[_]  = null,
  val resourceLoader: String => InputStream = null,
) extends Metadata with TypeMapper {

  val simpleTypeNameToXsdSimpleTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("xsd").orNull)
      .filter(_._2 != null)
      .toMap

  private val dbInfo = new TableMetadataDbInfo(tableDefs)
  import dbInfo.dbToTableDefs
  val dbSet = dbInfo.dbSet
  val db    = dbInfo.db
  val extraDbToMetadata: Map[String, TresqlMetadata] =
    dbToTableDefs
      .filter(_._1 != null)
      .transform { case (extraDb, tableDefs) =>
        if  (extraDb == db)
             this
        else TresqlMetadata(tableDefs, typeDefs, macrosClass) }
  val tables = dbToTableDefs.getOrElse(db, Nil).map { td =>
    def toTresqlCol(c: ColumnDef) = {
      val typeName = c.type_.name
      val scalaType = ExprType(xsd_scala_type_map(
        simpleTypeNameToXsdSimpleTypeName.getOrElse(typeName, typeName)).toString)
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
  private val tablesNormalized = tables.map { case (n, t) => (n.toLowerCase, t) }

  private val typeToVendorType: Map[String, Map[String, String]] =
    typeDefs.flatMap { t =>
      val vendorTypeName =
        t.ddlWrite.map { case (vendor, seqWi) =>
          vendor.replace(" sql", "") ->
            seqWi.head.targetNamePattern.replace("(size char)", "").replace("(size)", "")
      }
      Seq(
        t.name -> vendorTypeName,
        t.targetNames.getOrElse("scala", null) -> vendorTypeName
      ).filter(_._1 != null)
    }.filter(_._2.nonEmpty).toMap
  override def to_sql_type(vendor: String, typeName: String): String =
    typeToVendorType.get(typeName).flatMap(vt => vt.get(vendor).orElse(vt.get("sql"))) getOrElse typeName

  private val dbAsSuffix = Option(db).filter(_ != "") getOrElse "main-db"
  override val functionSignaturesResource: String =
    s"/tresql-function-signatures-$dbAsSuffix.txt"
  override val macroSignaturesResource: String =
    s"/tresql-macros-$dbAsSuffix.txt"
  override def getResourceAsStream(r: String): InputStream =
    if (resourceLoader == null) getClass.getResourceAsStream(r) else resourceLoader(r)

  override def table(name: String): Table =
    tables.getOrElse(name, tablesNormalized(name.toLowerCase))
  override def tableOption(name: String): Option[Table] =
    tables.get(name).orElse(tablesNormalized.get(name.toLowerCase))
  def tableOption(name: String, db: String): Option[Table] =
    if  (db == this.db)
         tableOption(name)
    else extraDbToMetadata(db).tableOption(name)
  lazy val tableMetadataString = {
    def colToString(col: ColumnDef) =
      col.name +
        (if (!col.nullable) " !" else "") +
        " " + simpleTypeNameToXsdSimpleTypeName.getOrElse(col.type_.name, col.type_.name) // XXX
    def refToString(cols: Seq[String], refTableName: String, refCols: Seq[String]) =
      cols.mkString(", ") + " -> " + refTableName + refCols.mkString("(", ", ", ")")
    import scala.language.implicitConversions
    implicit def stringToSeq(s: String): Seq[String] = Seq(s)
    def tableToString(table: Table, mojozTable: TableDef) =
      Seq[collection.Seq[String]](
        Option(mojozTable.db).map("db: " + _).toSeq,
        "table: " + table.name,
        "columns:",
        mojozTable.cols.map("- " + colToString(_)),
        if (table.key.cols.size > 0) "pk: " + table.key.cols.mkString(", ") else Nil,
        if (table.rfs.size > 0) "refs:" else Nil,
        table.rfs.toSeq.sortBy(_._1).flatMap(tr =>
          tr._2.map(r => "- " + refToString(r.cols, tr._1, r.refCols)))
      ).flatten.mkString("\n")
    (dbToTableDefs.getOrElse(db, Nil).sortBy(_.name) ++
     dbToTableDefs.toSeq.filter(_._1 != db).sortBy(_._1).flatMap(_._2.sortBy(_.name))
    ).map { t =>
      val table = if (t.db == db) tables(t.name) else extraDbToMetadata(t.db).tables(t.name)
      tableToString(table, t)
    }.mkString("\n\n") + "\n"
  }
}


class TresqlMetadataFactory extends CompilerMetadataFactory {
  /** Creates tresql compiler metadata by loading table metadata
   *  (as produced by [[querease.TresqlMetadata.tableMetadataString]]),
   *  function signatures and macros.
   *
   *  Inspects "table_metadata_resource" and "macros_class" keys in conf parameter.
   *  Defaults to /tresql-table-metadata.yaml and no macros class.
   *  Loads function signatures from first resource found in order:
   *    /tresql-function-signatures-<db-name>.txt,
   *    /tresql-function-signatures.txt,
   *    /tresql-default-function-signatures.txt.
   *  Loads macros from first resource found in order:
   *    /tresql-macros-<db-name>.txt,
   *    /tresql-macros.txt,
   *    /tresql-default-macros.txt.
   *  In resource names above, "main-db" is used for missing db-name.
   */
  override def create(conf: Map[String, String]): CompilerMetadata = {
    val tableMetadataResourceName =
      conf.getOrElse("table_metadata_resource", "/tresql-table-metadata.yaml")
    val macrosClassOpt = conf.get("macros_class").map(Class.forName)
    val rawTableMetadata = YamlMd.fromResource(tableMetadataResourceName)
    val mdConventions = new SimplePatternMdConventions(Nil, Nil, Nil)
    val typeDefs = TypeMetadata.defaultTypeDefs // XXX
    val tableDefs = new YamlTableDefLoader(rawTableMetadata, mdConventions, typeDefs).tableDefs
    val dbInfo = new TableMetadataDbInfo(tableDefs)
    val tresqlMetadata = TresqlMetadata(tableDefs, typeDefs = Nil, macrosClassOpt.orNull)
    new CompilerMetadata {
      override def metadata: Metadata = tresqlMetadata
      override def extraMetadata: Map[String, Metadata] = tresqlMetadata.extraDbToMetadata
      override def macros: Any =
        macrosClassOpt.map(_.getDeclaredConstructor().newInstance()).orNull
    }
  }
}

object TresqlMetadata {
  class TableMetadataDbInfo(tableDefs: Seq[TableDef]) {
    val dbToTableDefs: Map[String, Seq[TableDef]] = tableDefs.groupBy(_.db)
    val dbSet: Set[String] = dbToTableDefs.keySet
    val db = if (dbSet.contains(null)) null else tableDefs.headOption.map(_.db).orNull
  }
  /** Creates tresql compiler metadata from table metadata, typedefs and macros class.
   */
  def apply(
      tableDefs: Seq[TableDef],
      typeDefs: collection.immutable.Seq[TypeDef],
      macrosClass: Class[_]  = null,
      resourceLoader: String => InputStream = null,
  ): TresqlMetadata = {
    val dbInfo = new TableMetadataDbInfo(tableDefs)
    new TresqlMetadata(tableDefs, typeDefs, macrosClass, resourceLoader)
  }
}
