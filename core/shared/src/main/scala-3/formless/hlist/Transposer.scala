package formless.hlist

/**
 * Type class supporting transposing this `HList`.
 */
trait Transposer[L] extends DepFn1[L], Serializable

object Transposer {
  type Aux[L, O] = Transposer[L] { type Out = O }

  inline def apply[L](using t: Transposer[L]): Transposer.Aux[L, t.Out] = t

  given transposerHNil: Transposer.Aux[HNil, HNil] =
    new Transposer[HNil] {
      type Out = HNil
      def apply(l: HNil): Out = l
    }

  given transposerHList1[H <: HList, MC <: HList, Out0 <: HList](
    using mc: ConstMapper.Aux[HNil, H, MC],
    zo: ZipOne.Aux[H, MC, Out0],
  ): Transposer.Aux[H :: HNil, Out0] =
    new Transposer[H :: HNil] {
      type Out = Out0
      def apply(l: H :: HNil): Out = zo(l.head, mc(HNil, l.head))
    }

  given transposerHList2[H <: HList, TH <: HList, TT <: HList, OutT <: HList, Out0 <: HList](
    using tt: Transposer.Aux[TH :: TT, OutT],
    zo: ZipOne.Aux[H, OutT, Out0],
  ): Transposer.Aux[H :: TH :: TT, Out0] =
    new Transposer[H :: TH :: TT] {
      type Out = Out0
      def apply(l: H :: TH :: TT): Out = zo(l.head, tt(l.tail))
    }
}
