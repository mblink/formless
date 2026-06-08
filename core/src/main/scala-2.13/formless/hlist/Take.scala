package formless
package hlist

/**
 * Type class supporting retrieval of the first `N` elements of this `HList`. Available only if this `HList` has at
 * least `N` elements.
 */
trait Take[T, N] extends DepFn1[T] with Serializable

object Take {
  type Aux[T, N, O] = Take[T, N] { type Out = O }

  def apply[T, N](implicit d: Take[T, N]): Take.Aux[T, N, d.Out] = d

  implicit def takeHList[T <: HList, I <: Int, N <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    st: shapeless.ops.hlist.Take.Aux[T, N, O],
  ): Take.Aux[T, I, O] =
    new Take[T, I] {
      type Out = O
      def apply(t: T): Out = st(t)
    }
}
