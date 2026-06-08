package formless.record

import formless.hlist.{::, DepFn1, HList, HNil}

/**
 * Type class supporting converting this record to a `HList` of key-value pairs.
 */
trait Fields[T <: HList] extends DepFn1[T], Serializable

object Fields {
  type Aux[T <: HList, O] = Fields[T] { type Out = O }

  inline def apply[T <: HList](using v: Fields[T]): Fields.Aux[T, v.Out] = v

  given fieldsHNil[L <: HNil]: Fields.Aux[L, L] =
    new Fields[L] {
      type Out = L
      def apply(l: L) = l
    }

  given fieldsHCons[K, V, T <: HList](using k: ValueOf[K], t: Fields[T] { type Out <: HList }): Fields.Aux[(K ->> V) :: T, (K, V) :: t.Out] =
    new Fields[(K ->> V) :: T] {
      type Out = (K, V) :: t.Out
      def apply(l: (K ->> V) :: T) = (k.value -> l.head) :: t(l.tail)
    }
}
