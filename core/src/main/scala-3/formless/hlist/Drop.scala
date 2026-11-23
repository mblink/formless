package formless.hlist

/**
 * Type class supporting removal of the first `N` elements of this `HList`. The count must be between zero and the
 * length of the list, inclusive.
 */
trait Drop[T, N] extends DepFn1[T], Serializable

object Drop {
  type Aux[T, N, O] = Drop[T, N] { type Out = O }

  inline def apply[T, N](using d: Drop[T, N]): Drop.Aux[T, N, d.Out] = d

  given dropHList[T <: HList, N <: Int](
    using ev: HList.CountInBounds[T, N],
    n: ValueOf[N],
  ): Drop.Aux[T, N, HList.Drop[T, N]] =
    new Drop[T, N] {
      type Out = HList.Drop[T, N]
      def apply(t: T): Out = HList.drop(t, n.value)
    }
}
