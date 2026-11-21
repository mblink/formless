package formless.record

import formless.hlist.HList

/**
 * Type class to witness that a record of type `T` does not contain a key of type `K`.
 */
@scala.annotation.implicitNotFound("Cannot prove that record ${L} has no field with key ${K}: it has one, or one could be in an abstract part of the record (a type parameter or abstract type member)")
trait LacksKey[L <: HList, K]

object LacksKey {
  inline def apply[T <: HList, K](using l: LacksKey[T, K]): LacksKey[T, K] = l

  private val singleton = new LacksKey[HList, Any] {}

  given lacksKeyAbsent[T <: HList, K](using a: FieldAbsent[T, K ->> Any, <:<]): LacksKey[T, K] =
    singleton.asInstanceOf[LacksKey[T, K]]
}
