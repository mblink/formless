package formless.record

import formless.tuple.DepFn2

/**
 * Type class supporting modification of a record field by given function.
 */
trait Modifier[T <: Tuple, K, A, B] extends DepFn2[T, A => B] with Serializable

object Modifier {
  type Aux[T <: Tuple, K, A, B, O] = Modifier[T, K, A, B] { type Out = O }

  inline def apply[T <: Tuple, K, A, B](using m: Modifier[T, K, A, B]): Modifier.Aux[T, K, A, B, m.Out] = m

  inline given modifierInst[T <: Tuple, K, A, B](
    using ev: FieldValue[T, K] <:< A,
    idx: ValueOf[FieldIndex[T, K]],
  ): Modifier.Aux[T, K, A, B, ReplaceValue[T, K, B]] =
    new Modifier[T, K, A, B] {
      type Out = ReplaceValue[T, K, B]
      def apply(t: T, f: A => B): Out = {
        val a = t.toArray
        val i = idx.value
        a.update(i, f(a(i).asInstanceOf[A]).asInstanceOf[Object])
        Tuple.fromArray(a).asInstanceOf[Out]
      }
    }
}
