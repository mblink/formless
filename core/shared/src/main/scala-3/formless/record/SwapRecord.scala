package formless.record

import formless.tuple.DepFn0

/**
 * Type class supporting swapping the keys and values in a record of type `L`.
 */
trait SwapRecord[L <: Tuple] extends DepFn0 with Serializable

object SwapRecord {
  type Aux[L <: Tuple, O] = SwapRecord[L] { type Out = O }

  inline def apply[L <: Tuple](using s: SwapRecord[L]): SwapRecord.Aux[L, s.Out] = s

  given emptyTupleSwapRecord[L <: EmptyTuple]: Aux[L, EmptyTuple] =
    new SwapRecord[L] {
      type Out = EmptyTuple
      def apply(): Out = EmptyTuple
    }

  given tupleNSwapRecord[K, V, T <: Tuple](using k: ValueOf[K], t: SwapRecord[T] { type Out <: Tuple }): Aux[(K ->> V) *: T, (V ->> K) *: t.Out] =
    new SwapRecord[(K ->> V) *: T] {
      type Out = (V ->> K) *: t.Out
      def apply(): Out = label[V](k.value) *: t()
    }
}
