package test

trait Dto extends querease.Dto {

  override type QE = QuereaseTests.TestQuerease.type

  override protected def dbToPropName(name: String) =
    mojoz.metadata.Naming.camelizeLower(name)
  override protected def propToDbName(name: String) =
    mojoz.metadata.Naming.dbName(name)
}

trait DtoWithId extends Dto with querease.DtoWithId
