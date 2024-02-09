package formless.record

import formless.tuple.DepFn0

/**
 * Type class supporting collecting the keys of a record as a `Tuple`.
 */
trait Keys[T <: Tuple] extends DepFn0 with Serializable

object Keys {
  type Aux[T <: Tuple, Out0] = Keys[T] { type Out = Out0 }

  inline def apply[T <: Tuple](using k: Keys[T]): Keys.Aux[T, k.Out] = k

  given emptyTupleKeys[L <: EmptyTuple]: Keys.Aux[L, EmptyTuple] =
    new Keys[L] {
      type Out = EmptyTuple
      def apply(): Out = EmptyTuple
    }

  given tupleNKeys[K, V, T <: Tuple](using k: ValueOf[K], t: Keys[T] { type Out <: Tuple }): Keys.Aux[(K ->> V) *: T, K *: t.Out] =
    new Keys[(K ->> V) *: T] {
      type Out = K *: t.Out
      def apply(): Out = k.value *: t()
    }
}
