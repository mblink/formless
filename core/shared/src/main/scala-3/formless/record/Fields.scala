package formless.record

import formless.tuple.DepFn1

/**
 * Type class supporting converting this record to a `Tuple` of key-value pairs.
 */
trait Fields[T <: Tuple] extends DepFn1[T] with Serializable

object Fields {
  type Aux[T <: Tuple, O] = Fields[T] { type Out = O }

  inline def apply[T <: Tuple](using v: Fields[T]): Fields.Aux[T, v.Out] = v

  given emptyTupleFields[L <: EmptyTuple]: Fields.Aux[L, L] =
    new Fields[L] {
      type Out = L
      def apply(l: L) = l
    }

  given tupleNFields[K, V, T <: Tuple](using k: ValueOf[K], t: Fields[T] { type Out <: Tuple }): Fields.Aux[(K ->> V) *: T, (K, V) *: t.Out] =
    new Fields[(K ->> V) *: T] {
      type Out = (K, V) *: t.Out
      def apply(l: (K ->> V) *: T) = (k.value -> l.head) *: t(l.tail)
    }
}
