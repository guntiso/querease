package querease

import org.tresql.MetaData
import org.tresql.metadata.Col
import org.tresql.metadata.Key
import org.tresql.metadata.{ Ref => TresqlRef }
import org.tresql.metadata.Table

import metadata.TableDef

class TresqlMetadata(
  val tableDefs: Seq[TableDef],
  val procedureMetadata: MetaData)
  extends MetaData {

  val tables = tableDefs.map { td =>
    val name = td.name
    val cols = td.cols.map(c => Col(c.name, c.nullable)).toList
    val key = Key(td.pk.map(_.cols.toList) getOrElse Nil)
    val refs = td.refs.groupBy(_.refTable)
      .mapValues(_.map(r => TresqlRef(r.cols.toList)).toList)
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
