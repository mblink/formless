package formless.record

import formless.hlist.{::, HList, HNil}

/**
 * Type class combining `Keys` and `Values` for convenience.
 * Similar to `Fields`, but produces distinct `HList`s instead of a zipped one.
 */
trait UnzipFields[L] extends Serializable {
  type Keys
  type Values
  def keys: Keys
  def values(l: L): Values
}

object UnzipFields {
  type Aux[L, K, V] = UnzipFields[L] {
    type Keys = K
    type Values = V
  }

  inline def apply[L](using u: UnzipFields[L]): UnzipFields.Aux[L, u.Keys, u.Values] = u

  given unzipFieldsHNil[L <: HNil]: UnzipFields.Aux[L, HNil, L] =
    new UnzipFields[L] {
      type Keys = HNil
      type Values = L
      def keys = HNil
      def values(l: L): L = l
    }

  given unzipFieldsHCons[K, V, T <: HList](
    using k: ValueOf[K],
    t: UnzipFields[T] {
      type Keys <: HList
      type Values <: HList
    }
  ): UnzipFields.Aux[(K ->> V) :: T, K :: t.Keys, V :: t.Values] =
    new UnzipFields[(K ->> V) :: T] {
      type Keys = K :: t.Keys
      type Values = V :: t.Values

      def keys = k.value :: t.keys
      def values(l: (K ->> V) :: T) = l.head :: t.values(l.tail)
    }
}
