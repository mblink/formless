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
  inline def apply[T, K](t: T, @annotation.nowarn("msg=unused") k: K)(using r: Remover[T, K]): r.Out = r(t)

  given removerAt[T <: HList, K](using f: FieldOp[T, K ->> Any, FieldOp.Remove]): Remover.Aux[T, K, f.Out] =
    new Remover[T, K] {
      type Out = f.Out
      def apply(t: T): Out = HList.removedAt(t, f.index).asInstanceOf[Out]
    }
}
