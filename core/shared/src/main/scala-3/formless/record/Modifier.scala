package formless.record

import formless.tuple.DepFn2

/**
 * Type class supporting modification of a record field by given function.
 */
trait Modifier[T <: Tuple, K, A, B] extends DepFn2[T, A => B] with Serializable

object Modifier {
  type Aux[T <: Tuple, K, A, B, O] = Modifier[T, K, A, B] { type Out = O }

  inline def apply[T <: Tuple, K, A, B](using m: Modifier[T, K, A, B]): Modifier.Aux[T, K, A, B, m.Out] = m

  given modifierInst[T <: Tuple, K, A, B](using ff: FindField[T, K ->> A]): Modifier.Aux[T, K, A, B, ff.Replaced[K ->> B]] =
    new Modifier[T, K, A, B] {
      type Out = ff.Replaced[K ->> B]
      def apply(t: T, f: A => B): Out = {
        val a = t.toArray
        a.update(ff.index, f(a(ff.index).asInstanceOf[A]).asInstanceOf[Object])
        Tuple.fromArray(a).asInstanceOf[Out]
      }
    }
}
