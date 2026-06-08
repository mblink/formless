package formless.hlist

/**
 * Type class supporting left scanning of this `HList` with a binary polymorphic function.
 */
trait LeftScanner[L, In, F] extends DepFn2[L, In], Serializable

object LeftScanner{
  type Aux[L, In, F, O] = LeftScanner[L, In, F] { type Out = O }

  inline def apply[L, In, F](using s: LeftScanner[L, In, F]): LeftScanner.Aux[L, In, F, s.Out] = s

  given leftScannerHNil[In, F]: LeftScanner.Aux[HNil, In, F, In :: HNil] =
    new LeftScanner[HNil, In, F] {
      type Out = In :: HNil
      def apply(l: HNil, in: In) = in :: HNil
    }

  given leftScannerHCons[H, T <: HList, In, F, OutP, ScanOut <: HList](
    using ch: Case2.Aux[F, H, In, OutP],
    st: LeftScanner.Aux[T, OutP, F, ScanOut],
  ): LeftScanner.Aux[H :: T, In, F, In :: ScanOut] =
    new LeftScanner[H :: T, In, F] {
      type Out = In :: ScanOut
      def apply(l: H :: T, in: In) = in :: st(l.tail, ch(l.head, in))
    }
}
