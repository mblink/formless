package formless
package hlist

/**
 * Type class supporting rotating a `HList` right.
 */
trait RotateRight[L, N] extends DepFn1[L] with Serializable

object RotateRight {
  type Aux[L, N, O] = RotateRight[L, N] { type Out = O }

  def apply[L, N](implicit r: RotateRight[L, N]): RotateRight.Aux[L, N, r.Out] = r

  implicit def rotateRightHList[L <: HList, I <: Int, N <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sr: shapeless.ops.hlist.RotateRight.Aux[L, N, O],
  ): RotateRight.Aux[L, I, O] =
    new RotateRight[L, I] {
      type Out = O
      def apply(l: L): Out = sr(l)
    }
}
