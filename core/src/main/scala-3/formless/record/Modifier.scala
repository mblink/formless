package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting modification of a record field by given function.
 */
trait Modifier[T <: HList, K, A, B] extends DepFn2[T, A => B], Serializable

object Modifier {
  type Aux[T <: HList, K, A, B, O] = Modifier[T, K, A, B] { type Out = O }

  inline def apply[T <: HList, K, A, B](using m: Modifier[T, K, A, B]): Modifier.Aux[T, K, A, B, m.Out] = m

  given modifierAt[T <: HList, K, A, B](using op: FieldOp[T, K ->> A, FieldOp.Modify[K, B]]): Modifier.Aux[T, K, A, B, op.Out] =
    new Modifier[T, K, A, B] {
      type Out = op.Out
      def apply(t: T, f: A => B): Out = HList.updatedAt(t, op.index, a => f(a.asInstanceOf[A])).asInstanceOf[Out]
    }
}
