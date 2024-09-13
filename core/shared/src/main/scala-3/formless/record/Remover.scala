package formless
package record

import formless.hlist.{DepFn1, HList}

/**
 * Type class supporting record field removal.
 */
trait Remover[T, K] extends DepFn1[T], Serializable

object Remover {
  type Aux[T, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T, K](using r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T, K](t: T, k: K)(using r: Remover[T, K]): r.Out = r(t)

  given removerInst[T <: HList, K](
    using f: FindField[T, K ->> Any, <:<]
  ): Remover.Aux[T, K, (f.Value, f.Removed)] =
    new Remover[T, K] {
      type Out = (f.Value, f.Removed)
      def apply(t: T): Out = {
        val a = t.toArray
        (a(f.index), HList.fromArray(a.patch(f.index, Nil, 1))).asInstanceOf[Out]
      }
    }
}
