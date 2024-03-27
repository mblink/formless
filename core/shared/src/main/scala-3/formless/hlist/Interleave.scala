package formless.hlist

/**
 * Type class supporting adding an element to each possible position in this `HList`.
 */
trait Interleave[A, L] extends DepFn2[A, L] with Serializable

object Interleave {
  type Aux[A, L, O] = Interleave[A, L] { type Out = O }

  inline def apply[A, L](using i: Interleave[A, L]): Interleave.Aux[A, L, i.Out] = i

  given interleaveHNil[A, L <: HNil]: Interleave.Aux[A, L, (A :: HNil) :: HNil] =
    new Interleave[A, L] {
      type Out = (A :: HNil) :: HNil
      def apply(a: A, l: L): Out = (a :: HNil) :: HNil
    }

  given interleaveHCons[A, H, T <: HList, LI <: HList, MapConsOut <: HList](
    using it: Interleave.Aux[A, T, LI],
    mh: MapCons.Aux[H, LI, MapConsOut],
  ): Interleave.Aux[A, H :: T, (A :: H :: T) :: MapConsOut] =
    new Interleave[A, H :: T] {
      type Out = (A :: H :: T) :: MapConsOut
      def apply(a: A, l: H :: T): Out = (a :: l) :: mh(l.head, it(a, l.tail))
    }
}
