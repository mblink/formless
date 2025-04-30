package formless
package hlist

/**
 * Type class supporting supporting access to the elements in range [a,b] of this `HList`.
 * Available only if this `HList` contains all elements in range.
 */
trait SelectRange[L, A, B] extends DepFn1[L] with Serializable

object SelectRange {
  type Aux[L, A, B, O] = SelectRange[L, A, B] { type Out = O }

  def apply[L, A, B](implicit s: SelectRange[L, A, B]): SelectRange.Aux[L, A, B, s.Out] = s

  implicit def selectRangeHList[L <: HList, A <: Int, AN <: shapeless.Nat, B <: Int, BN <: shapeless.Nat, O <: HList](
    implicit @annotation.unused ain: IntToNat.Aux[A, AN],
    @annotation.unused bin: IntToNat.Aux[B, BN],
    sr: shapeless.ops.hlist.SelectRange.Aux[L, AN, BN, O],
  ): SelectRange.Aux[L, A, B, O] =
    new SelectRange[L, A, B] {
      type Out = O
      def apply(l: L): Out = sr(l)
    }
}
