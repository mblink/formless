package formless.record

import formless.hlist.{::, HList, HNil}
import scala.reflect.TypeTest

trait FromMap[R] extends Serializable {
  def apply[K, V](m: Map[K, V]): Option[R]
}

object FromMap {
  inline def apply[R](using f: FromMap[R]): FromMap[R] = f

  given fromMapHNil: FromMap[HNil] =
    new FromMap[HNil] {
      def apply[K, V](m: Map[K, V]): Option[HNil] = Some(HNil)
    }

  given fromMapHCons[K0, V0, T <: HList](
    using k: ValueOf[K0],
    tv: TypeTest[Any, V0],
    fmt: FromMap[T],
  ): FromMap[(K0 ->> V0) :: T] =
    new FromMap[(K0 ->> V0) :: T] {
      def apply[K, V](m: Map[K, V]): Option[(K0 ->> V0) :: T] =
        for {
          v <- m.get(k.value.asInstanceOf[K])
          v0 <- tv.unapply(v)
          t <- fmt(m)
        } yield label[K0](v0) :: t
    }
}
