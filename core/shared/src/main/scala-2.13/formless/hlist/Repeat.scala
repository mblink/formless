package formless
package hlist

/**
 * Typeclass supporting repeating a `HList` of type `L` `N` times.
 */
trait Repeat[L, N] extends DepFn1[L] with Serializable

object Repeat {
  type Aux[L, N, O] = Repeat[L, N] { type Out = O }

  def apply[L, N](implicit r: Repeat[L, N]): Repeat.Aux[L, N, r.Out] = r

  implicit def repeatHList[L <: HList, I <: Int, N <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sr: shapeless.ops.hlist.Repeat.Aux[L, N, O],
  ): Repeat.Aux[L, I, O] =
    new Repeat[L, I] {
      type Out = O
      def apply(l: L): Out = sr(l)
    }
}
