package formless
package hlist

/**
 * Type class supporting producing a `HList` of length `N` filled with elements of type `A`.
 */
trait Fill[N, A] extends DepFn1[A] with Serializable

object Fill {
  type Aux[N, A, O] = Fill[N, A] { type Out = O }

  def apply[N, A](implicit f: Fill[N, A]): Fill.Aux[N, A, f.Out] = f

  final class FillPartialAp[N](private val dummy: Boolean = false) extends AnyVal {
    final def apply[A](a: A)(implicit f: Fill[N, A]): f.Out = f(a)
  }

  def apply[N]: FillPartialAp[N] = new FillPartialAp[N]

  implicit def fillHList[I <: Int, N <: shapeless.Nat, A, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sf: shapeless.ops.hlist.Fill.Aux[N, A, O],
  ): Fill.Aux[I, A, O] =
    new Fill[I, A] {
      type Out = O
      def apply(a: A): Out = sf(a)
    }

  implicit def fillHList2[I <: Int, J <: Int, NI <: shapeless.Nat, NJ <: shapeless.Nat, A, O <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, NI],
    @annotation.unused jn: IntToNat.Aux[J, NJ],
    sf: shapeless.ops.hlist.Fill.Aux[(NI, NJ), A, O],
  ): Fill.Aux[(I, J), A, O] =
    new Fill[(I, J), A] {
      type Out = O
      def apply(a: A): Out = sf(a)
    }
}
