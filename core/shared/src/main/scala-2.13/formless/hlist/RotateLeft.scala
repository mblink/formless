package formless
package hlist

/**
 * Type class supporting rotating a `HList` left.
 */
trait RotateLeft[L, N] extends DepFn1[L] with Serializable

object RotateLeft {
  type Aux[L, N, O] = RotateLeft[L, N] { type Out = O }

  def apply[L, N](implicit r: RotateLeft[L, N]): RotateLeft.Aux[L, N, r.Out] = r

  implicit def rotateLeftHList[L <: HList, I <: Int, N <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sr: shapeless.ops.hlist.RotateLeft.Aux[L, N, O],
  ): RotateLeft.Aux[L, I, O] =
    new RotateLeft[L, I] {
      type Out = O
      def apply(l: L): Out = sr(l)
    }
}
