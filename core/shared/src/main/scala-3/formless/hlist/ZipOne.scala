package formless.hlist

/**
 * Type class supporting zipping this `HList` with a `HList` of `HList`s returning a `HList` of `HList`s with each
 * element of this `HList` prepended to the corresponding `HList` element of the argument `HList`.
 */
trait ZipOne[H, T] extends DepFn2[H, T] with Serializable

sealed trait ZipOneLP {
  final type Aux[H, T, O] = ZipOne[H, T] { type Out = O }

  final given zipOne1[H]: ZipOne.Aux[H, HNil, HNil] =
    new ZipOne[H, HNil] {
      type Out = HNil
      def apply(h: H, t: HNil): Out = HNil
    }

  final given zipOne2[T]: ZipOne.Aux[HNil, T, HNil] =
    new ZipOne[HNil, T] {
      type Out = HNil
      def apply(h: HNil, t: T): Out = HNil
    }

  final given zipOne4[HH, HT <: HList, TH <: HList, TT <: HList, ZotOut <: HList](
    using zot: ZipOne.Aux[HT, TT, ZotOut],
  ): ZipOne.Aux[HH :: HT, TH :: TT, (HH :: TH) :: ZotOut] =
    new ZipOne[HH :: HT, TH :: TT] {
      type Out = (HH :: TH) :: ZotOut
      def apply(h: HH :: HT, t: TH :: TT): Out = (h.head :: t.head) :: zot(h.tail, t.tail)
    }
}

object ZipOne extends ZipOneLP {
  inline def apply[H <: HList, T <: HList](using z: ZipOne[H, T]): ZipOne.Aux[H, T, z.Out] = z

  given zipOne0: ZipOne.Aux[HNil, HNil, HNil] =
    new ZipOne[HNil, HNil] {
      type Out = HNil
      def apply(h: HNil, t: HNil): Out = HNil
    }

  given zipOne3[H, T <: HList]: ZipOne.Aux[H :: HNil, T :: HNil, (H :: T) :: HNil] =
    new ZipOne[H :: HNil, T :: HNil] {
      type Out = (H :: T) :: HNil
      def apply(h: H :: HNil, t: T :: HNil): Out = (h.head :: t.head) :: HNil
    }
}

