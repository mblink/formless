package formless.hlist

/**
 * Type class supporting replacement of the first element of type `U` in this `HList` with the result of
 * its transformation via a given function into a new element of type `V`.
 * Available only if this `HList` contains an element of type `U`.
 */
trait Modifier[L, U, V] extends DepFn2[L, U => V] with Serializable

object Modifier {
  type Aux[L, U, V, O] = Modifier[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Modifier[L, U, V]): Modifier.Aux[L, U, V, r.Out] = r

  given modifierHList[T <: HList, U, V](using ff: FindField[T, U]): Modifier.Aux[T, U, V, (U, ff.Replaced[V])] =
    new Modifier[T, U, V] {
      type Out = (U, ff.Replaced[V])
      def apply(t: T, f: U => V): Out = {
        val a = t.toArray
        val u = a(ff.index).asInstanceOf[U]
        a.update(ff.index, f(u))
        (u, HList.fromArray(a)).asInstanceOf[Out]
      }
    }
}
