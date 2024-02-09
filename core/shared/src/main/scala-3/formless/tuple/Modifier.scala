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

  inline given tupleModifier1[T <: Tuple, U, V]: Modifier.Aux[U *: T, U, V, (U, V *: T)] =
    new Modifier[U *: T, U, V] {
      type Out = (U, V *: T)
      def apply(l : U *: T, f : U => V): Out = {
        val u = l.head
        (u, f(u) *: l.tail)
      }
    }

  inline given tupleModifier2[H, T <: Tuple, U, V, OutT <: Tuple](
    using t: Modifier.Aux[T, U, V, (U, OutT)],
  ): Modifier.Aux[H *: T, U, V, (U, H *: OutT)] =
    new Modifier[H *: T, U, V] {
      type Out = (U, H *: OutT)
      def apply(l : H *: T, f : U => V): Out = {
        val (u, outT) = t(l.tail, f)
        (u, l.head *: outT)
      }
    }
}
