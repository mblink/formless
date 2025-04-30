package formless
package hlist

/**
 * Type class supporting computing the singleton `Int` corresponding to the length of this `HList`.
 */
trait Length[T <: HList] extends DepFn0 with Serializable { type Out <: Int }

object Length {
  type Aux[T <: HList, O <: Int] = Length[T] { type Out = O }

  def apply[T <: HList](implicit l: Length[T]): Length.Aux[T, l.Out] = l

  implicit def lengthHList[T <: HList, N <: shapeless.Nat, I <: Int](
    implicit @annotation.unused sl: shapeless.ops.hlist.Length.Aux[T, N],
    ni: NatToInt.Aux[N, I],
  ): Length.Aux[T, I] =
    new Length[T] {
      type Out = I
      def apply(): I = ni()
    }
}
