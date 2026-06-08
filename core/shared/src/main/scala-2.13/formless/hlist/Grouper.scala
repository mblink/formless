package formless
package hlist

/**
 * Typeclass supporting grouping this `HList` into `HList`s of `N` items each, at `Step` apart.
 * If `Step` equals `N` then the groups do not overlap.
 */
trait Grouper[L, N, Step] extends DepFn1[L] with Serializable

object Grouper {
  type Aux[L, N, Step, O] = Grouper[L, N, Step] { type Out = O }

  def apply[L, N, Step](implicit g: Grouper[L, N, Step]): Grouper.Aux[L, N, Step, g.Out] = g

  implicit def grouperHList[L <: HList, N <: Int, Step <: Int, NN <: shapeless.Nat, StepN <: shapeless.Nat, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[N, NN],
    @annotation.unused stepN: IntToNat.Aux[Step, StepN],
    sg: shapeless.ops.hlist.Grouper.Aux[L, NN, StepN, O],
  ): Grouper.Aux[L, N, Step, O] =
    new Grouper[L, N, Step] {
      type Out = O
      def apply(l: L): Out = sg(l)
    }
}
