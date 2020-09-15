package test

import org.mojoz.querease

trait Dto extends querease.Dto { self =>
  override type QE = QuereaseTests.TestQuerease.type
}

trait DtoWithId extends Dto with querease.DtoWithId
