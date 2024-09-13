package formless.hlist

import scala.compiletime.ops.int.S

/**
 * Type class supporting access to the `N`th element of this `HList`. Available only if this `HList` has at least `N` elements.
 */
trait At[L, N] extends DepFn1[L], Serializable

object At {
  type Aux[L, N, O] = At[L, N] { type Out = O }

  inline def apply[L, N](using a: At[L, N]): At.Aux[L, N, a.Out] = a

  given hlistAtZero[H, T <: HList]: At.Aux[H :: T, 0, H] =
    new At[H :: T, 0] {
      type Out = H
      def apply(l: H :: T): Out = l.head
    }

  given hlistAtN[H, T <: HList, N <: Int, O](using a: At.Aux[T, N, O]): At.Aux[H :: T, S[N], O] =
    new At[H :: T, S[N]] {
      type Out = O
      def apply(l: H :: T): Out = a(l.tail)
    }
}
