package formless
package hlist

/**
 * Type class supporting replacement of the `N`th element of this `HList` with the result of calling `F` on it.
 * Available only if this `HList` contains at least `N` elements.
 */
trait ModifierAt[L, N, U, V] extends DepFn2[L, U => V] with Serializable

object ModifierAt {
  type Aux[L, N, U, V, O] = ModifierAt[L, N, U, V] { type Out = O }

  def apply[L, N, U, V](implicit r: ModifierAt[L, N, U, V]): ModifierAt.Aux[L, N, U, V, r.Out] = r

  implicit def modifierAtHList[L <: HList, I <: Int, N <: shapeless.Nat, U, V, O](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sm: shapeless.ops.hlist.ModifierAt.Aux[L, N, U, V, O],
  ): ModifierAt.Aux[L, I, U, V, O] =
    new ModifierAt[L, I, U, V] {
      type Out = O
      def apply(l: L, f: U => V): Out = sm(l, f)
    }
}
