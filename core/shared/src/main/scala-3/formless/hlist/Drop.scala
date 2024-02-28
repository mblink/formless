package formless.hlist

import compiletime.ops.int.>=

/**
 * Type class supporting removal of the first `N` elements of this `HList`. Available only if this `HList` has at least `N` elements.
 */
trait Drop[T, N] extends DepFn1[T] with Serializable

object Drop {
  type Aux[T, N, O] = Drop[T, N] { type Out = O }

  inline def apply[T, N](using d: Drop[T, N]): Drop.Aux[T, N, d.Out] = d

  given dropHList[T <: HList, N <: Int](
    using ev: (HList.Size[T] >= N) =:= true,
    n: ValueOf[N],
  ): Drop.Aux[T, N, HList.Drop[T, N]] =
    new Drop[T, N] {
      type Out = HList.Drop[T, N]
      def apply(t: T): Out = HList.drop(t, n.value)
    }
}
