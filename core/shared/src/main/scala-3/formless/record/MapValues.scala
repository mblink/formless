package formless.record

import formless.tuple.{Case1, DepFn1}

/**
 * Type class supporting mapping a higher rank function over the values of a record.
 */
trait MapValues[F, L] extends DepFn1[L] with Serializable

object MapValues {
  type Aux[F, L, O] = MapValues[F, L] { type Out = O }

  inline def apply[F, L](using m: MapValues[F, L]): MapValues.Aux[F, L, m.Out] = m

  given emptyTupleMapValues[F, L <: EmptyTuple]: MapValues.Aux[F, L, EmptyTuple] =
    new MapValues[F, L] {
      type Out = EmptyTuple
      def apply(l: L) = EmptyTuple
    }

  given tupleNMapValues[F, K, V, T <: Tuple](
    using ch: Case1[F, V],
    mt: MapValues[F, T] { type Out <: Tuple },
  ): MapValues.Aux[F, (K ->> V) *: T, (K ->> ch.Result) *: mt.Out] =
    new MapValues[F, (K ->> V) *: T] {
      type Out = (K ->> ch.Result) *: mt.Out
      def apply(l: (K ->> V) *: T) = label[K](ch(l.head: V)) *: mt(l.tail)
    }
}
