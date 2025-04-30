package formless
package hlist

/**
 * Type class supporting replacement of the `N`th element of this `HList` with an element of type `V`.
 * Available only if this `HList` contains at least `N` elements.
 */
trait ReplaceAt[L, N, V] extends DepFn2[L, V] with Serializable

object ReplaceAt {
  type Aux[L, N, V, O] = ReplaceAt[L, N, V] { type Out = O }

  def apply[L, N, V](implicit r: ReplaceAt[L, N, V]): ReplaceAt.Aux[L, N, V, r.Out] = r

  implicit def replaceAtHList[L <: HList, I <: Int, N <: shapeless.Nat, V, O](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sr: shapeless.ops.hlist.ReplaceAt.Aux[L, N, V, O],
  ): ReplaceAt.Aux[L, I, V, O] =
    new ReplaceAt[L, I, V] {
      type Out = O
      def apply(l: L, v: V): Out = sr(l, v)
    }
}
