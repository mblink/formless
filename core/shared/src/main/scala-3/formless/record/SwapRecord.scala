package formless.record

import formless.hlist.{::, DepFn0, HList, HNil}

/**
 * Type class supporting swapping the keys and values in a record of type `L`.
 */
trait SwapRecord[L <: HList] extends DepFn0 with Serializable

object SwapRecord {
  type Aux[L <: HList, O] = SwapRecord[L] { type Out = O }

  inline def apply[L <: HList](using s: SwapRecord[L]): SwapRecord.Aux[L, s.Out] = s

  given swapRecordHNil[L <: HNil]: Aux[L, HNil] =
    new SwapRecord[L] {
      type Out = HNil
      def apply(): Out = HNil
    }

  given swapRecordHCons[K, V, T <: HList](using k: ValueOf[K], t: SwapRecord[T] { type Out <: HList }): Aux[(K ->> V) :: T, (V ->> K) :: t.Out] =
    new SwapRecord[(K ->> V) :: T] {
      type Out = (V ->> K) :: t.Out
      def apply(): Out = label[V](k.value) :: t()
    }
}
