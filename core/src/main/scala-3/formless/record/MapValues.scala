package formless.record

import formless.hlist.{::, Case1, DepFn1, HList, HNil}

/**
 * Type class supporting mapping a higher rank function over the values of a record.
 */
trait MapValues[F, L] extends DepFn1[L], Serializable

object MapValues {
  type Aux[F, L, O] = MapValues[F, L] { type Out = O }

  inline def apply[F, L](using m: MapValues[F, L]): MapValues.Aux[F, L, m.Out] = m

  given mapValuesHNil[F, L <: HNil]: MapValues.Aux[F, L, HNil] =
    new MapValues[F, L] {
      type Out = HNil
      def apply(l: L) = HNil
    }

  given mapValuesHCons[F, K, V, T <: HList](
    using ch: Case1[F, V],
    mt: MapValues[F, T] { type Out <: HList },
  ): MapValues.Aux[F, (K ->> V) :: T, (K ->> ch.Result) :: mt.Out] =
    new MapValues[F, (K ->> V) :: T] {
      type Out = (K ->> ch.Result) :: mt.Out
      def apply(l: (K ->> V) :: T) = label[K](ch(l.head: V)) :: mt(l.tail)
    }
}
