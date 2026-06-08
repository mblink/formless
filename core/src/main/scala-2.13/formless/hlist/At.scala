package formless
package hlist

import scala.language.implicitConversions

/**
 * Type class supporting access to the `N`th element of this `HList`. Available only if this `HList` has at least `N` elements.
 */
trait At[L, N] extends DepFn1[L] with Serializable

object At {
  type Aux[L, N, O] = At[L, N] { type Out = O }

  def apply[L, N](implicit a: At[L, N]): At.Aux[L, N, a.Out] = a

  implicit def atHList[L <: HList, I <: Int, N <: shapeless.Nat, O](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    sa: shapeless.ops.hlist.At.Aux[L, N, O],
  ): At.Aux[L, I, O] =
    new At[L, I] {
      type Out = O
      def apply(l: L): Out = sa(l)
    }

  trait WithInt[L] {
    type N <: Int
    type O
    val instance: At.Aux[L, N, O]
  }

  object WithInt {
    type Aux[L, N0 <: Int, O0] = WithInt[L] {
      type N = N0
      type O = O0
    }

    implicit def apply[L, O0](n: Int)(implicit a: At.Aux[L, n.type, O0]): WithInt.Aux[L, n.type, O0] =
      new WithInt[L] {
        type N = n.type
        type O = O0
        val instance = a
      }
  }
}
