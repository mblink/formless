package formless.hlist

import formless.record.{->>, label}

/**
 * Type class supporting zipping a `HList` of values with a `HList` of keys to create a record.
 */
trait ZipWithKeys[K, V] extends DepFn1[V], Serializable

object ZipWithKeys {
  type Aux[K, V, O] = ZipWithKeys[K, V] { type Out = O }

  inline def apply[K, V](using z: ZipWithKeys[K, V]): ZipWithKeys.Aux[K, V, z.Out] = z

  object F extends Poly2 {
    given kv[K, V]: Case.Aux[K, V, K ->> V] = at((_, v) => label[K](v))
  }

  inline given zipWithKeysHList[K <: HList, V <: HList](
    using z: ZipWith[K, V, F.type],
  ): ZipWithKeys.Aux[K, V, z.Out] =
    new ZipWithKeys[K, V] {
      type Out = z.Out
      def apply(v: V): Out = z(constValueHList[K], v)
    }
}
