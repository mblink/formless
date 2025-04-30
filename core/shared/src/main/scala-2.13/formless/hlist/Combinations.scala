package formless
package hlist

/**
 * Type class supporting the calculation of every combination of this `HList`
 */
trait Combinations[N, L] extends DepFn1[L] with Serializable

object Combinations {
  type Aux[N, L, O] = Combinations[N, L] { type Out = O }

  def apply[N, L](implicit c: Combinations[N, L]): Combinations.Aux[N, L, c.Out] = c
  def apply[N, L](@annotation.unused n: N, l: L)(implicit c: Combinations[N, L]): c.Out = c(l)

  implicit def combinationsHList[I <: Int, N <: shapeless.Nat, L <: HList, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sc: shapeless.ops.hlist.Combinations.Aux[N, L, O],
  ): Combinations.Aux[I, L, O] =
    new Combinations[I, L] {
      type Out = O
      def apply(l: L): Out = sc(l)
    }
}
