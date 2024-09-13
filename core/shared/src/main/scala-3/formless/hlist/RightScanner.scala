package formless.hlist

/**
 * Type class supporting right scanning of this `HList` with a binary polymorphic function.
 */
trait RightScanner[L, In, F] extends DepFn2[L, In], Serializable

object RightScanner {
  type Aux[L, In, F, O] = RightScanner[L, In, F] { type Out = O }

  inline def apply[L, In, F](using s: RightScanner[L, In, F]): RightScanner.Aux[L, In, F, s.Out] = s

  trait RightScanner0[L, V, F] extends DepFn2[L, V]

  object RightScanner0 {
    type Aux[L, V, F, O] = RightScanner0[L, V, F] { type Out = O }

    given rightScannerHCons1[H, H0, T <: HList, F, C2Result](
      using ev: Case2.Aux[F, H0, H, C2Result],
    ): RightScanner0.Aux[H :: T, H0, F, C2Result :: H :: T] =
      new RightScanner0[H :: T, H0, F] {
        type Out = C2Result :: H :: T
        def apply(l: H :: T, h: H0) = ev(h, l.head) :: l
      }
  }

  given rightScannerHNil[In, F]: RightScanner.Aux[HNil, In, F, In :: HNil] =
    new RightScanner[HNil, In, F] {
      type Out = In :: HNil
      def apply(l: HNil, in: In): Out = in :: HNil
    }

  given rightScannerHCons[H, T <: HList, In, F, R <: HList, Scan0Out <: HList](
    using st: RightScanner.Aux[T, In, F, R],
    sh: RightScanner0.Aux[R, H, F, Scan0Out],
  ): RightScanner.Aux[H :: T, In, F, Scan0Out] =
    new RightScanner[H :: T, In, F] {
      type Out = Scan0Out
      def apply(l: H :: T, in: In) = sh(st(l.tail, in), l.head)
    }
}
