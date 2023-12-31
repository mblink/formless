package formless.record

import formless.tuple.DepFn0

type SwapRecordT[L <: Tuple] <: Tuple = L match {
  case (k ->> v) *: t => (v ->> k) *: SwapRecordT[t]
  case EmptyTuple => EmptyTuple
}

/**
 * Type class supporting swapping the keys and values in a record of type `L`.
 */
trait SwapRecord[L <: Tuple] extends DepFn0 with Serializable { final type Out = SwapRecordT[L] }

object SwapRecord {
  type Aux[L <: Tuple, O] = SwapRecord[L] { type Out = O }

  inline def apply[L <: Tuple](using s: SwapRecord[L]): SwapRecord.Aux[L, s.Out] = s

  given tupleSwapRecord[L <: Tuple](using k: Keys[L]): SwapRecord.Aux[L, SwapRecordT[L]] =
    new SwapRecord[L] {
      def apply(): Out = k().asInstanceOf[Out]
    }
}
