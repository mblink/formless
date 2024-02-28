package formless.hlist

import compiletime.ops.int.>=

/**
 * Type class supporting retrieval of the first `N` elements of this `HList`. Available only if this `HList` has at
 * least `N` elements.
 */
trait Take[T, N] extends DepFn1[T] with Serializable

object Take {
  type Aux[T, N, O] = Take[T, N] { type Out = O }

  inline def apply[T, N](using t: Take[T, N]): Take.Aux[T, N, t.Out] = t

  given takeHList[T <: HList, N <: Int](
    using ev: (HList.Size[T] >= N) =:= true,
    n: ValueOf[N],
  ): Take.Aux[T, N, HList.Take[T, N]] =
    new Take[T, N] {
      type Out = HList.Take[T, N]
      def apply(t: T): Out = HList.take(t, n.value)
    }
}
