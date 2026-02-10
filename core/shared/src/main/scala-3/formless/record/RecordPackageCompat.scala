package formless
package record

import formless.hlist.HList
import scala.language.implicitConversions

trait RecordPackageCompat {
  final opaque type ->>[K, +V] = V
  sealed trait `->>LP` {
    @deprecated("Use inlineConvertToV", "0.8.0")
    private[record] implicit def convertToV[K, V](kv: K ->> V): V = kv
  }
  object ->> extends `->>LP` {
    inline implicit def inlineConvertToV[K, V](kv: K ->> V): V = kv
  }

  @inline final def field[K]: [V] => V => (K ->> V) = [v] => (v: v) => v
  @inline final def label[K]: [V] => V => (K ->> V) = [v] => (v: v) => v

  extension[K <: Singleton](k: K) {
    inline final def ->>[V](v: V): K ->> V = v
  }

  extension[K, V](kv: K ->> V) {
    inline final def label(using k: ValueOf[K]): K = k.value
  }

  final implicit def toFormlessRecordOps[T <: HList](t: T): FormlessRecordOps[T] = new FormlessRecordOps(t)

  final implicit def toFormlessMapOps[K, V](m: Map[K, V]): FormlessMapOps[K, V] = new FormlessMapOps(m)
}
