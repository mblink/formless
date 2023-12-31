package formless.tuple

import compiletime.ops.int.{-, >=, Max}

/**
 * Type class supporting padding a `Tuple` of type `L` to length `N`, padded with elements of type `A`.
 */
trait PadTo[N, A, L] extends DepFn2[A, L] with Serializable

object PadTo {
  type Aux[N, A, L, O] = PadTo[N, A, L] { type Out = O }

  inline def apply[N, A, L](using p: PadTo[N, A, L]): PadTo.Aux[N, A, L, p.Out] = p

  given tuplePadTo[N <: Int, A, L <: Tuple](
    using ev: (N >= Tuple.Size[L]) =:= true,
    n: ValueOf[N],
    size: ValueOf[Tuple.Size[L]],
  ): PadTo.Aux[N, A, L, Tuple.Concat[L, FillT[Max[0, N - Tuple.Size[L]], A]]] =
    new PadTo[N, A, L] {
      type Out = Tuple.Concat[L, FillT[Max[0, N - Tuple.Size[L]], A]]
      def apply(a: A, l: L): Out =
        (l ++ Fill.fill(math.max(0, n.value - size.value), a, EmptyTuple)).asInstanceOf[Out]
    }
}
