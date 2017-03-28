package test

trait Dto extends querease.Dto {
  override protected lazy val metadata =
    QuereaseTests.dtoMetadata
  override protected def dbToPropName(name: String) =
    mojoz.metadata.out.ScalaClassWriter.scalaFieldName(name)
  override protected def propToDbName(name: String) =
    mojoz.metadata.Naming.dbName(name)
}

trait DtoWithId extends Dto with querease.DtoWithId
