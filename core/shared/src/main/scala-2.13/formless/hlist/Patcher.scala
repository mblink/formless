package formless
package hlist

/**
 * Type class supporting the patching of a `HList`.
 */
trait Patcher[N, M, L, In] extends DepFn2[L, In] with Serializable

object Patcher {
  type Aux[N, M, L, In, O] = Patcher[N, M, L, In] { type Out = O }

  def apply[N, M, L, In](implicit p: Patcher[N, M, L, In]): Patcher.Aux[N, M, L, In, p.Out] = p

  implicit def patchHList[N <: Int, M <: Int, NN <: shapeless.Nat, MN <: shapeless.Nat, L <: HList, In <: HList, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[N, NN],
    @annotation.unused im: IntToNat.Aux[M, MN],
    sp: shapeless.ops.hlist.Patcher.Aux[NN, MN, L, In, O],
  ): Patcher.Aux[N, M, L, In, O] =
    new Patcher[N, M, L, In] {
      type Out = O
      def apply(l: L, in: In): Out = sp(l, in)
    }
}
