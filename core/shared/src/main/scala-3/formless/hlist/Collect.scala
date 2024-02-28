package formless.hlist

/**
 * Type class witnessing that a `HList` can be collected with a `Poly` of type `F` to produce a `HList`
 */
trait Collect[I, F] extends DepFn1[I] with Serializable

sealed trait CollectLP {
  final type Aux[L, F, O] = Collect[L, F] { type Out = O }

  final given noPolyCaseHCons[LH, LT <: HList, P <: Poly, CollectOut <: HList](
    using ct: Collect.Aux[LT, P, CollectOut],
  ): Collect.Aux[LH :: LT, P, CollectOut] =
    new Collect[LH :: LT, P] {
      type Out = CollectOut
      def apply(l: LH :: LT): Out = ct(l.tail)
    }
}

object Collect extends CollectLP {
  inline def apply[L, F](using c: Collect[L, F]): Collect.Aux[L, F, c.Out] = c

  given collectHNil[P <: Poly]: Collect.Aux[HNil, P, HNil] =
    new Collect[HNil, P] {
      type Out = HNil
      def apply(l: HNil): Out = HNil
    }

  given collectHCons[LH, LT <: HList, P <: Poly, CollectOut <: HList, ClrResult](
    using ct: Collect.Aux[LT, P, CollectOut],
    ch: Case1.Aux[P, LH, ClrResult],
  ): Collect.Aux[LH :: LT, P, ClrResult :: CollectOut] =
    new Collect[LH :: LT, P] {
      type Out = ClrResult :: CollectOut
      def apply(l: LH :: LT): Out = ch(l.head) :: ct(l.tail)
    }
}
