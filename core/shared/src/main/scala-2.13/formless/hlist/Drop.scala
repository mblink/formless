package formless
package hlist

/**
 * Type class supporting removal of the first `N` elements of this `HList`. Available only if this `HList` has at least `N` elements.
 */
trait Drop[T, N] extends DepFn1[T] with Serializable

object Drop {
  type Aux[T, N, O] = Drop[T, N] { type Out = O }

  def apply[T, N](implicit d: Drop[T, N]): Drop.Aux[T, N, d.Out] = d

  implicit def dropHList[T <: HList, I <: Int, N <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sd: shapeless.ops.hlist.Drop.Aux[T, N, O],
  ): Drop.Aux[T, I, O] =
    new Drop[T, I] {
      type Out = O
      def apply(t: T): Out = sd(t)
    }
}
