package test

object TestQuereaseNaming {
  def dbToPropName(name: String) =
    mojoz.metadata.Naming.camelizeLower(name)
  def propToDbName(name: String) =
    mojoz.metadata.Naming.dbName(name)
}

trait Dto extends querease.Dto { self =>

  override type QE = QuereaseTests.TestQuerease.type

  override protected def dbToPropName(name: String) =
    TestQuereaseNaming.dbToPropName(name)
  override protected def propToDbName(name: String) =
    TestQuereaseNaming.propToDbName(name)
}

trait DtoWithId extends Dto with querease.DtoWithId
