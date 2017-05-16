package querease

import org.tresql.Result
import org.tresql.RowLike

trait QuereaseIo { this: Querease =>

  type DTO <: AnyRef

  type CloseableResult[B <: DTO] = Iterator[B] with AutoCloseable

  def convertRow[B <: DTO: Manifest](row: RowLike): B
  def toSaveableMap[B <: DTO: Manifest](instance: B): Map[String, _]
  def keyMap[B <: DTO: Manifest](instance: B): Map[String, _]
}
