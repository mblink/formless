package formless
package record

import formless.tuple.DepFn1

/**
 * Type class supporting record field removal.
 */
trait Remover[T, K] extends DepFn1[T] with Serializable

object Remover {
  type Aux[T, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T, K](using r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T, K](t: T, k: K)(using r: Remover[T, K]): r.Out = r(t)

  given removerInst[T <: Tuple, K](
    using f: FindField[T, K ->> Any, <:<]
  ): Remover.Aux[T, K, (f.Value, f.Removed)] =
    new Remover[T, K] {
      type Out = (f.Value, f.Removed)
      def apply(t: T): Out = {
        val a = t.toArray
        (a(f.index), Tuple.fromArray(a.patch(f.index, Nil, 1))).asInstanceOf[Out]
      }
    }
}
