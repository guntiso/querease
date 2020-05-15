package test

trait Dto extends querease.Dto { self =>
  override type QE = QuereaseTests.TestQuerease.type
}

trait DtoWithId extends Dto with querease.DtoWithId
