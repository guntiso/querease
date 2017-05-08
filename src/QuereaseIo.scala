package querease

import org.tresql.Result
import org.tresql.RowLike

trait QuereaseIo { this: Querease =>

  type DTO <: AnyRef

  def fromRows[B <: DTO: Manifest](rows: Result[RowLike]): List[B]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
  def keyMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
}
