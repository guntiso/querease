package test

trait Dto extends QuereaseTests.qe.Dto {
  override protected def dbToPropName(name: String) =
    mojoz.metadata.out.ScalaClassWriter.scalaFieldName(name)
  override protected def propToDbName(name: String) =
    mojoz.metadata.Naming.dbName(name)
}

trait DtoWithId extends Dto with QuereaseTests.qe.DtoWithId
