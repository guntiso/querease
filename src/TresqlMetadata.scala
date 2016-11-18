package querease

import org.tresql.MetaData
import org.tresql.metadata.Col
import org.tresql.metadata.Key
import org.tresql.metadata.{ Ref => TresqlRef }
import org.tresql.metadata.Table
import org.tresql.metadata.TypeMapper

import mojoz.metadata.Type
import mojoz.metadata.TableDef.{ TableDefBase => TableDef }
import mojoz.metadata.ColumnDef.{ ColumnDefBase => ColumnDef }

class TresqlMetadata(
  val tableDefs: Seq[TableDef[ColumnDef[Type]]],
  val procedureMetadata: MetaData)
  extends MetaData with TypeMapper {

  val tables = tableDefs.map { td =>
    def toTresqlCol(c: ColumnDef[Type]) = {
      val typeName = c.type_.name
      val scalaType = xsd_scala_type_map(typeName)
      val jdbcTypeCode = 0 // unknown, not interested
      Col(c.name, c.nullable, jdbcTypeCode, scalaType)
    }
    val name = td.name
    val cols = td.cols.map(toTresqlCol).toList
    val key = Key(td.pk.map(_.cols.toList) getOrElse Nil)
    val refs = td.refs.groupBy(_.refTable)
      .mapValues(_.map(r => TresqlRef(r.cols.toList, r.refCols.toList)).toList)
    Table(name, cols, key, refs)
  }.map(t => (t.name, t)).toMap

  override def table(name: String) = tables(name)
  override def tableOption(name: String) = tables.get(name)
  override def procedure(name: String) =
    if (procedureMetadata != null) procedureMetadata.procedure(name)
    else sys.error("Procedure metadata not initialized")
  override def procedureOption(name: String) =
    if (procedureMetadata != null) procedureMetadata.procedureOption(name)
    else None
}
