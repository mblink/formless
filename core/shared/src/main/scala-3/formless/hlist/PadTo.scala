package formless.hlist

import compiletime.ops.int.{-, >=, Max}

/**
 * Type class supporting padding a `HList` of type `L` to length `N`, padded with elements of type `A`.
 */
trait PadTo[N, A, L] extends DepFn2[A, L], Serializable

object PadTo {
  type Aux[N, A, L, O] = PadTo[N, A, L] { type Out = O }

  inline def apply[N, A, L](using p: PadTo[N, A, L]): PadTo.Aux[N, A, L, p.Out] = p

  given padToHList[N <: Int, A, L <: HList](
    using ev: (N >= HList.Size[L]) =:= true,
    n: ValueOf[N],
    size: ValueOf[HList.Size[L]],
  ): PadTo.Aux[N, A, L, HList.Concat[L, HList.Fill[Max[0, N - HList.Size[L]], A]]] =
    new PadTo[N, A, L] {
      type Out = HList.Concat[L, HList.Fill[Max[0, N - HList.Size[L]], A]]
      def apply(a: A, l: L): Out =
        (l ++ HList.fill0(math.max(0, n.value - size.value), a, HNil)).asInstanceOf[Out]
    }
}
