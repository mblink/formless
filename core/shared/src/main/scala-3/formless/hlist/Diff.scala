package formless.hlist

/**
 * Type class supporting `HList` subtraction. In case of duplicate types, this operation is a multiset difference.
 * If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList` contains the last n - m
 * elements of type `T` in this `HList`.
 *
 * Also available if `M` contains types absent in this `HList`.
 */
trait Diff[L, M] extends DepFn1[L], Serializable

sealed trait DiffLP {
  final type Aux[L, M, O] = Diff[L, M] { type Out = O }

  final given diffHCons1[L, H, T <: HList, D <: HList](using d: Diff.Aux[L, T, D]): Diff.Aux[L, H :: T, D] =
    new Diff[L, H :: T] {
      type Out = D
      def apply(l: L): Out = d(l)
    }
}

object Diff extends DiffLP {
  inline def apply[L, M](using d: Diff[L, M]): Diff.Aux[L, M, d.Out] = d

  given diffHNil[L]: Diff.Aux[L, HNil, L] =
    new Diff[L, HNil] {
      type Out = L
      def apply(l: L): Out = l
    }

  given diffHCons2[L <: HList, LT, H, T <: HList, D](
    using r: Remove.Aux[L, H, (H, LT)],
    d: Diff.Aux[LT, T, D]
  ): Diff.Aux[L, H :: T, D] =
    new Diff[L, H :: T] {
      type Out = D
      def apply(l: L): Out = d(r(l)._2)
    }
}
