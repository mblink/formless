package formless.hlist

/**
 * Type class supporting unification of this `HList`.
 */
trait Unifier[T] extends DepFn1[T] with Serializable

object Unifier {
  type Aux[T, O] = Unifier[T] { type Out = O }

  inline def apply[T](using u: Unifier[T]): Unifier.Aux[T, u.Out] = u

  given unifierHNil: Unifier.Aux[HNil, HNil] =
    new Unifier[HNil] {
      type Out = HNil
      def apply(l: HNil): Out = l
    }

  given unifierHCons1[T]: Unifier.Aux[T :: HNil, T :: HNil] =
    new Unifier[T :: HNil] {
      type Out = T :: HNil
      def apply(l: T :: HNil): Out = l
    }

  given unifierHCons[H1, H2, L, T <: HList, LtOut <: HList](
    using lub: Lub[H1, H2, L],
    ut: Unifier.Aux[L :: T, LtOut],
  ): Unifier.Aux[H1 :: H2 :: T, L :: LtOut] =
    new Unifier[H1 :: H2 :: T] {
      type Out = L :: LtOut
      def apply(l: H1 :: H2 :: T): Out = lub.left(l.head) :: ut(lub.right(l.tail.head) :: l.tail.tail)
    }
}
