package formless.hlist

import scala.util.NotGiven

/**
 * Type class supporting unification of all elements that are subtypes of `B` in this `HList` to `B`, with all other
 * elements left unchanged.
 */
trait SubtypeUnifier[T, B] extends DepFn1[T] with Serializable

object SubtypeUnifier {
  type Aux[T, B, O] = SubtypeUnifier[T, B] { type Out = O }

  inline def apply[T, B](using u: SubtypeUnifier[T, B]): SubtypeUnifier.Aux[T, B, u.Out] = u

  given subtypeUnifierHNil[B]: SubtypeUnifier.Aux[HNil, B, HNil] =
    new SubtypeUnifier[HNil, B] {
      type Out = HNil
      def apply(l: HNil): Out = l
    }

  given subtypeUnifierHCons1[H, T <: HList, B, SutOut <: HList](
    using st: H <:< B,
    sut: SubtypeUnifier.Aux[T, B, SutOut],
  ): SubtypeUnifier.Aux[H :: T, B, B :: SutOut] =
    new SubtypeUnifier[H :: T, B] {
      type Out = B :: SutOut
      def apply(l: H :: T): Out = st(l.head) :: sut(l.tail)
    }

  given subtypeUnifierHCons1[H, T <: HList, B, SutOut <: HList](
    using nst: NotGiven[H <:< B],
    sut: SubtypeUnifier.Aux[T, B, SutOut],
  ): SubtypeUnifier.Aux[H :: T, B, H :: SutOut] =
    new SubtypeUnifier[H :: T, B] {
      type Out = H :: SutOut
      def apply(l: H :: T): Out = l.head :: sut(l.tail)
    }
}
