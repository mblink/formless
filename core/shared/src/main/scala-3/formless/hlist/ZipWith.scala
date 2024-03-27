package formless.hlist

/**
 * Type class supporting zipping a `HList` with another `HList` using a `Poly` resulting in a `HList`
 */
trait ZipWith[L, R, F] extends DepFn2[L, R] with Serializable

object ZipWith {
  type Aux[L, R, F, O] = ZipWith[L, R, F] { type Out = O }

  inline def apply[L, R, F](using z: ZipWith[L, R, F]): ZipWith.Aux[L, R, F, z.Out] = z

  private def constZipWith[L, R, P]: ZipWith.Aux[L, R, P, HNil] =
    new ZipWith[L, R, P] {
      type Out = HNil
      def apply(l: L, r: R): HNil = HNil
    }

  given hnilZipWithHNil[F]: ZipWith.Aux[HNil, HNil, F, HNil] = constZipWith[HNil, HNil, F]
  given hnilZipWithHList[R, F]: ZipWith.Aux[HNil, R, F, HNil] = constZipWith[HNil, R, F]
  given hlistZipWithHNil[L, F]: ZipWith.Aux[L, HNil, F, HNil] = constZipWith[L, HNil, F]

  given hlistZipWithHList[LH, RH, LT <: HList, RT <: HList, F, ZipWithOut <: HList, ClrResult](
    using zt: ZipWith.Aux[LT, RT, F, ZipWithOut],
    c: Case2.Aux[F, LH, RH, ClrResult],
  ): ZipWith.Aux[LH :: LT, RH :: RT, F, ClrResult :: ZipWithOut] =
    new ZipWith[LH :: LT, RH :: RT, F] {
      type Out = ClrResult :: ZipWithOut
      def apply(l: LH :: LT, r: RH :: RT): Out =
        c(l.head, r.head) :: zt(l.tail, r.tail)
    }
}
