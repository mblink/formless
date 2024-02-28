package formless.hlist

/**
 * Type class supporting interleaving an element into each row of this `HList` of `HList`s.
 */
trait FlatMapInterleave[A, M] extends DepFn2[A, M] with Serializable

object FlatMapInterleave {
  type Aux[A, M, O] = FlatMapInterleave[A, M] { type Out = O }

  inline def apply[A, M](using i: FlatMapInterleave[A, M]): FlatMapInterleave.Aux[A, M, i.Out] = i

  given flatMapInterleaveHNil[A, M <: HNil]: FlatMapInterleave.Aux[A, M, HNil] =
    new FlatMapInterleave[A, M] {
      type Out = HNil
      def apply(a: A, m: M): Out = HNil
    }

  given flatMapInterleaveHCons[A, H <: HList, TM <: HList, HO <: HList, TMO <: HList, PrependOut <: HList](
    using ih: Interleave.Aux[A, H, HO],
    it: FlatMapInterleave.Aux[A, TM, TMO],
    p: Prepend.Aux[HO, TMO, PrependOut]
  ): FlatMapInterleave.Aux[A, H :: TM, PrependOut] =
    new FlatMapInterleave[A, H :: TM] {
      type Out = PrependOut
      def apply(a: A, m: H :: TM): Out =
        p(ih(a, m.head), it(a, m.tail))
    }
}
