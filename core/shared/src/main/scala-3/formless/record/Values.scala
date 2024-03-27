package formless.record

import formless.hlist.{::, DepFn1, HList, HNil}

/**
 * Type class supporting collecting the values of a record as a `HList`.
 */
trait Values[T <: HList] extends DepFn1[T] with Serializable

object Values {
  type Aux[T <: HList, O] = Values[T] { type Out = O }

  inline def apply[T <: HList](using v: Values[T]): Values.Aux[T, v.Out] = v

  given valuesHNil[L <: HNil]: Values.Aux[L, HNil] =
    new Values[L] {
      type Out = HNil
      def apply(l: L): Out = HNil
    }

  given valuesHCons[K, V, T <: HList](using t: Values[T] { type Out <: HList }): Values.Aux[(K ->> V) :: T, V :: t.Out] =
    new Values[(K ->> V) :: T] {
      type Out = V :: t.Out
      def apply(l: (K ->> V) :: T): Out = (l.head: V) :: t(l.tail)
    }
}
