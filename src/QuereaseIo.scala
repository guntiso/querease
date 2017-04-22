package querease

import org.tresql.Result
import org.tresql.RowLike

trait QuereaseIo { this: Querease =>

  def fromRows[T <: AnyRef](rows: Result[RowLike], clazz: Class[T]): List[T]
  def toSaveableMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
  def keyMap(instance: AnyRef, viewDef: ViewDef): Map[String, _]
}
