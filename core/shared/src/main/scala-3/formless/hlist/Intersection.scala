package formless.hlist

/**
 * Type class supporting `HList` intersection. In case of duplicate types, this operation is a multiset intersection.
 * If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList` contains the first m
 * elements of type `T` in this `HList`.
 *
 * Also available if `M` contains types absent in this `HList`.
 */
trait Intersection[L, M] extends DepFn1[L] with Serializable

object Intersection {
  type Aux[L, M, O] = Intersection[L, M] { type Out = O }

  inline def apply[L, M](using i: Intersection[L, M]): Intersection.Aux[L, M, i.Out] = i

  // let ∅ ∩ M = ∅
  given intersectionHNil[M <: HList]: Intersection.Aux[HNil, M, HNil] =
    new Intersection[HNil, M] {
      type Out = HNil
      def apply(l: HNil): Out = HNil
    }

  // let (H :: T) ∩ M = T ∩ M when H ∉ M
  given intersectionHCons1[H, T <: HList, M, I <: HList](
    using f: NotContains[M, H],
    i: Intersection.Aux[T, M, I],
  ): Intersection.Aux[H :: T, M, I] =
    new Intersection[H :: T, M] {
      type Out = I
      def apply(l: H :: T): Out = i(l.tail)
    }

  // let (H :: T) ∩ M  =  H :: (T ∩ (M - H)) when H ∈ M
  given intersectionHCons1[H, T <: HList, M, MR, I <: HList](
    using r: Remove.Aux[M, H, (H, MR)],
    i: Intersection.Aux[T, MR, I]
  ): Intersection.Aux[H :: T, M, H :: I] =
    new Intersection[H :: T, M] {
      type Out = H :: I
      def apply(l: H :: T): Out = l.head :: i(l.tail)
    }
}
