package formless.hlist

/**
 * Type class supporting `HList` union. In case of duplicate types, this operation is a order-preserving multi-set union.
 * If type `T` appears n times in this `HList` and m > n times in `M`, the resulting `HList` contains the first n elements
 * of type `T` in this `HList`, followed by the last m - n element of type `T` in `M`.
 */
trait Union[L, M] extends DepFn2[L, M] with Serializable

object Union {
  type Aux[L, M, O] = Union[L, M] { type Out = O }

  inline def apply[L, M](using u: Union[L, M]): Union.Aux[L, M, u.Out] = u

  // let ∅ ∪ M = M
  given unionHNil[M <: HList]: Union.Aux[HNil, M, M] =
    new Union[HNil, M] {
      type Out = M
      def apply(l: HNil, m: M): Out = m
    }

  // let (H :: T) ∪ M = H :: (T ∪ M) when H ∉ M
  given unionHCons1[H, T <: HList, M, U <: HList](
    using f: NotContains[M, H],
    u: Union.Aux[T, M, U]
  ): Union.Aux[H :: T, M, H :: U] =
    new Union[H :: T, M] {
      type Out = H :: U
      def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
    }

  // let (H :: T) ∪ M  =  H :: (T ∪ (M - H)) when H ∈ M
  given unionHCons2[H, T <: HList, M, MR, U <: HList](
    using r: Remove.Aux[M, H, (H, MR)],
    u: Union.Aux[T, MR, U]
  ): Union.Aux[H :: T, M, H :: U] =
    new Union[H :: T, M] {
      type Out = H :: U
      def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, r(m)._2)
    }
}
