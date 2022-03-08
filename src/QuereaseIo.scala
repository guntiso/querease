package org.mojoz.querease

import org.tresql.RowLike

trait QuereaseIo { this: Querease =>

  type DTO <: AnyRef

  type CloseableResult[+B <: DTO] = Iterator[B] with AutoCloseable

  def convertRow[B <: DTO: Manifest](row: RowLike): B
  def toMap[B <: DTO](instance: B): Map[String, _]
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1")
  def toSaveableMap[B <: DTO](instance: B): Map[String, _]
  def keyMap[B <: DTO](instance: B): Map[String, _]
}
