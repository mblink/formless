package formless.hlist

/**
 * Type class supporting that a `HList` of type `T` contains a set of elements of type `S`.
 */
trait SelectAll[L, S] extends DepFn1[L] with Serializable { type Out = S }

object SelectAll {
  inline def apply[L, S](using s: SelectAll[L, S]): SelectAll[L, S] = s

  given selectAllHNil[L]: SelectAll[L, HNil] =
    new SelectAll[L, HNil] {
      def apply(l: L): Out = HNil
    }

  given selectAllHCons[L <: HList, H, S <: HList](
    using sh: Selector[L, H],
    st: SelectAll[L, S],
  ): SelectAll[L, H :: S] =
    new SelectAll[L, H :: S] {
      def apply(l: L): Out = sh(l) :: st(l)
    }
}
