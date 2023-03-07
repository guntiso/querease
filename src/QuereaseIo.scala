package org.mojoz.querease

import org.tresql.RowLike

trait QuereaseIo[-DTO] {
  def convertRow[B <: DTO: Manifest](row: RowLike): B
  def toMap[B <: DTO](instance: B): Map[String, _]
}
