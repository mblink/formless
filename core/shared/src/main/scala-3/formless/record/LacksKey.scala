package formless.record

import scala.util.NotGiven

/**
 * Type class to witness that a record of type `T` does not contain a key of type `K`.
 */
trait LacksKey[L <: Tuple, K]

object LacksKey {
  inline def apply[T <: Tuple, K](using l: LacksKey[T, K]): LacksKey[T, K] = l

  private val singleton = new LacksKey[Tuple, Any] {}

  given lacksKeyInst[T <: Tuple, K](using nf: NotGiven[FindField[T, K ->> Any]]): LacksKey[T, K] =
    singleton.asInstanceOf[LacksKey[T, K]]
}
