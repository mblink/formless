package formless
package hlist

/**
 * Type class supporting the patching of a `HList`.
 */
trait Range[N, M] extends DepFn0 with Serializable

object Range {
  type Aux[N, M, O] = Range[N, M] { type Out = O }

  def apply[N, M](implicit r: Range[N, M]): Range.Aux[N, M, r.Out] = r

  object natToInt extends Poly1 {
    implicit def inst[N <: shapeless.Nat, I <: Int](implicit ni: NatToInt.Aux[N, I]): Case.Aux[N, I] = at(_ => ni())
  }

  implicit def rangeInst[N <: Int, M <: Int, NN <: shapeless.Nat, MN <: shapeless.Nat, O <: HList, MO <: HList](
    implicit @annotation.unused in: IntToNat.Aux[N, NN],
    @annotation.unused mn: IntToNat.Aux[M, MN],
    sr: shapeless.ops.nat.Range.Aux[NN, MN, O],
    m: Mapper.Aux[natToInt.type, O, MO],
  ): Range.Aux[N, M, MO] =
    new Range[N, M] {
      type Out = MO
      def apply(): Out = m(sr())
    }
}
