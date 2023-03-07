package org.mojoz.querease

import org.tresql.RowLike

trait QuereaseIo[DTO] {

  type CloseableResult[+B <: DTO] = Iterator[B] with AutoCloseable

  def convertRow[B <: DTO: Manifest](row: RowLike): B
  def toMap[B <: DTO](instance: B): Map[String, _]
  @deprecated("Results of this method are not used and this method will be removed, use toMap", "6.1.0")
  def toSaveableMap[B <: DTO](instance: B): Map[String, _]
  @deprecated("Results of this method are not used and this method will be removed", "6.1.0")
  def keyMap[B <: DTO](instance: B): Map[String, _]
}
