package formless.record

import formless.hlist.HList
import scala.util.NotGiven

/**
 * Type class to witness that a record of type `T` does not contain a key of type `K`.
 */
trait LacksKey[L <: HList, K]

object LacksKey {
  inline def apply[T <: HList, K](using l: LacksKey[T, K]): LacksKey[T, K] = l

  private val singleton = new LacksKey[HList, Any] {}

  @deprecated("Retained for binary compatibility", "0.8.1")
  private[record] def lacksKeyInst[T <: HList, K](using nf: NotGiven[FindField[T, K ->> Any, <:<]]): LacksKey[T, K] =
    singleton.asInstanceOf[LacksKey[T, K]]

  given lacksKeyAbsent[T <: HList, K](using a: FieldAbsent[T, K ->> Any, <:<]): LacksKey[T, K] =
    singleton.asInstanceOf[LacksKey[T, K]]
}
