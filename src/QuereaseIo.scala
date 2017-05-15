package querease

import org.tresql.Result
import org.tresql.RowLike

trait QuereaseIo { this: Querease =>

  type DTO <: AnyRef

  def fromRows[B <: DTO: Manifest](rows: Result[RowLike]): Stream[B]
  def toSaveableMap[B <: DTO: Manifest](instance: B): Map[String, _]
  def keyMap[B <: DTO: Manifest](instance: B): Map[String, _]
}
