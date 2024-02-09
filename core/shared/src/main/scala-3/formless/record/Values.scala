package formless.record

import formless.tuple.DepFn1

/**
 * Type class supporting collecting the values of a record as a `Tuple`.
 */
trait Values[T <: Tuple] extends DepFn1[T] with Serializable

object Values {
  type Aux[T <: Tuple, O] = Values[T] { type Out = O }

  inline def apply[T <: Tuple](using v: Values[T]): Values.Aux[T, v.Out] = v

  given emptyTupleValues[L <: EmptyTuple]: Values.Aux[L, EmptyTuple] =
    new Values[L] {
      type Out = EmptyTuple
      def apply(l: L): Out = EmptyTuple
    }

  given tupleNValues[K, V, T <: Tuple](using t: Values[T] { type Out <: Tuple }): Values.Aux[(K ->> V) *: T, V *: t.Out] =
    new Values[(K ->> V) *: T] {
      type Out = V *: t.Out
      def apply(l: (K ->> V) *: T): Out = (l.head: V) *: t(l.tail)
    }
}
