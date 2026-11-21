package formless.hlist

/**
 * Type class supporting replacement of the first element of type `U` in this `HList` with the result of
 * its transformation via a given function into a new element of type `V`.
 * Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot modify an element of type ${U} in ${L}: no element has that type, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait Modifier[L, U, V] extends DepFn2[L, U => V], Serializable

object Modifier {
  type Aux[L, U, V, O] = Modifier[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Modifier[L, U, V]): Modifier.Aux[L, U, V, r.Out] = r

  given modifierAt[T <: HList, U, V](using op: formless.record.ReplaceAt[T, U, formless.record.ReplaceAt.Elem]): Modifier.Aux[T, U, V, (U, op.Replaced[V])] =
    new Modifier[T, U, V] {
      type Out = (U, op.Replaced[V])
      def apply(t: T, f: U => V): Out = HList.replacedAt(t, op.index, f).asInstanceOf[Out]
    }
}
