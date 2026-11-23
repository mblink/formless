package formless.hlist

/**
 * Type class supporting producing a `HList` filled from a `Poly` of type `F`.
 */
trait FillWith[F, L] extends DepFn0, Serializable { final type Out = L }

object FillWith {
  inline def apply[F, L](using f: FillWith[F, L]): FillWith[F, L] = f

  given fillWithHNil[F]: FillWith[F, HNil] =
    new FillWith[F, HNil] {
      def apply(): Out = HNil
    }

  given fillWithHCons[F <: Poly, H, T <: HList](using h: Case0.Aux[F, H], t: FillWith[F, T]): FillWith[F, H :: T] =
    new FillWith[F, H :: T] {
      def apply(): Out = h() :: t()
    }
}
