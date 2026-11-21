package formless.hlist

/**
 * Type class supporting retrieval of the first `N` elements of this `HList`. The count must be between zero and
 * the length of the list, inclusive.
 */
trait Take[T, N] extends DepFn1[T], Serializable

object Take {
  type Aux[T, N, O] = Take[T, N] { type Out = O }

  inline def apply[T, N](using t: Take[T, N]): Take.Aux[T, N, t.Out] = t

  given takeHList[T <: HList, N <: Int](
    using ev: HList.CountInBounds[T, N],
    n: ValueOf[N],
  ): Take.Aux[T, N, HList.Take[T, N]] =
    new Take[T, N] {
      type Out = HList.Take[T, N]
      def apply(t: T): Out = HList.take(t, n.value)
    }
}
