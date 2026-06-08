package formless.record

import formless.hlist.{::, DepFn0, HList, HNil}

/**
 * Type class supporting collecting the keys of a record as a `HList`.
 */
trait Keys[T <: HList] extends DepFn0, Serializable

object Keys {
  type Aux[T <: HList, Out0] = Keys[T] { type Out = Out0 }

  inline def apply[T <: HList](using k: Keys[T]): Keys.Aux[T, k.Out] = k

  given keysHNil[L <: HNil]: Keys.Aux[L, HNil] =
    new Keys[L] {
      type Out = HNil
      def apply(): Out = HNil
    }

  given keysHCons[K, V, T <: HList](using k: ValueOf[K], t: Keys[T] { type Out <: HList }): Keys.Aux[(K ->> V) :: T, K :: t.Out] =
    new Keys[(K ->> V) :: T] {
      type Out = K :: t.Out
      def apply(): Out = k.value :: t()
    }
}
