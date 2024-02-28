package formless.record

import formless.hlist.{::, DepFn1, HList, HNil}

/**
 * Type class reordering record `T` by the `HList` of keys `K`.
 */
trait AlignByKeys[T, K] extends DepFn1[T] with Serializable

object AlignByKeys {
  type Aux[T, K, O] = AlignByKeys[T, K] { type Out = O }

  inline def apply[T, K](using a: AlignByKeys[T, K]): AlignByKeys.Aux[T, K, a.Out] = a

  given alignByKeysHNil: AlignByKeys.Aux[HNil, HNil, HNil] =
    new AlignByKeys[HNil, HNil] {
      type Out = HNil
      def apply(t: HNil): HNil = HNil
    }

  given alignByKeysHCons[T <: HList, KH, KT <: HList, V, R <: HList, TA <: HList](
    using rh: Remover.Aux[T, KH, (V, R)],
    at: AlignByKeys.Aux[R, KT, TA]
  ): AlignByKeys.Aux[T, KH :: KT, (KH ->> V) :: TA] =
    new AlignByKeys[T, KH :: KT] {
      type Out = (KH ->> V) :: TA
      def apply(t: T): (KH ->> V) :: TA = {
        val (v, r) = rh(t)
        label[KH](v) :: at(r)
      }
    }
}
