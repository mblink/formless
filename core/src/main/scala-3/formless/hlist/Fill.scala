package formless.hlist

import compiletime.ops.int.>=

/**
 * Type class supporting producing a `HList` of length `N` filled with elements of type `A`.
 */
trait Fill[N, A] extends DepFn1[A], Serializable

object Fill {
  type Aux[N, A, O] = Fill[N, A] { type Out = O }

  inline def apply[N, A](using f: Fill[N, A]): Fill.Aux[N, A, f.Out] = f

  final class FillPartialAp[N](private val dummy: Boolean = false) extends AnyVal {
    inline def apply[A](a: A)(using f: Fill[N, A]): f.Out = f(a)
  }

  inline def apply[N]: FillPartialAp[N] = new FillPartialAp[N]

  given fillHList[N <: Int, A](
    using ev: (N >= 0) =:= true,
    n: ValueOf[N],
  ): Fill.Aux[N, A, HList.Fill[N, A]] =
    new Fill[N, A] {
      type Out = HList.Fill[N, A]
      def apply(a: A): Out = HList.fill0[N, A, HNil](n.value, a, HNil)
    }

  given fillHList2[N <: Int, M <: Int, A](
    using fillM: Fill[M, A],
    fillN: Fill[N, fillM.Out],
  ): Fill.Aux[(N, M), A, fillN.Out] =
    new Fill[(N, M), A] {
      type Out = fillN.Out
      def apply(a: A): Out = fillN(fillM(a))
    }
}
