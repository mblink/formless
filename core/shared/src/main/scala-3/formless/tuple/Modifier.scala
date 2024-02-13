package formless.tuple

/**
 * Type class supporting replacement of the first element of type `U` in this `Tuple` with the result of
 * its transformation via a given function into a new element of type `V`.
 * Available only if this `Tuple` contains an element of type `U`.
 */
trait Modifier[L, U, V] extends DepFn2[L, U => V] with Serializable

object Modifier {
  type Aux[L, U, V, O] = Modifier[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Modifier[L, U, V]): Modifier.Aux[L, U, V, r.Out] = r

  given tupleModifier[T <: Tuple, U, V](using ff: FindField[T, U]): Modifier.Aux[T, U, V, (U, ff.Replaced[V])] =
    new Modifier[T, U, V] {
      type Out = (U, ff.Replaced[V])
      def apply(t: T, f: U => V): Out = {
        val a = t.toArray
        val u = a(ff.index).asInstanceOf[U]
        a.update(ff.index, f(u).asInstanceOf[Object])
        (u, Tuple.fromArray(a)).asInstanceOf[Out]
      }
    }
}
