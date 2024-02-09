package formless
package tuple

type ReplaceAtIndex[L <: Tuple, I <: Int, A] = ReplaceAtIndex0[L, I, A, 0]

type ReplaceAtIndex0[L <: Tuple, I <: Int, A, Curr <: Int] <: Tuple = (L, Curr) match {
  case (_ *: t, I) => A *: t
  case (h *: t, _) => h *: ReplaceAtIndex0[t, I, A, compiletime.ops.int.S[Curr]]
}

/**
 * Type class supporting replacement of the first element of type `U` from this `Tuple` with an element of type `V`.
 * Available only if this `Tuple` contains an element of type `U`.
 */
trait Replacer[L, U, V] extends DepFn2[L, V] with Serializable

object Replacer {
  type Aux[L, U, V, O] = Replacer[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Replacer[L, U, V]): Replacer.Aux[L, U, V, r.Out] = r

  given tupleReplacer1[T <: Tuple, U, V]: Replacer.Aux[U *: T, U, V, (U, V *: T)] =
    new Replacer[U *: T, U, V] {
      type Out = (U, V *: T)
      def apply(l : U *: T, v : V): Out = (l.head, v *: l.tail)
    }

  given tupleReplacer2[H, T <: Tuple, U, V, OutT <: Tuple](
    using t: Replacer.Aux[T, U, V, (U, OutT)],
  ): Replacer.Aux[H *: T, U, V, (U, H *: OutT)] =
      new Replacer[H *: T, U, V] {
        type Out = (U, H *: OutT)
        def apply(l : H *: T, v : V): Out = {
          val (u, outT) = t(l.tail, v)
          (u, l.head *: outT)
        }
      }
}
