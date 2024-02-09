package formless.record

import scala.reflect.TypeTest

trait FromMap[R] extends Serializable {
  def apply[K, V](m: Map[K, V]): Option[R]
}

object FromMap {
  inline def apply[R](using f: FromMap[R]): FromMap[R] = f

  given emptyTupleFromMap: FromMap[EmptyTuple] =
    new FromMap[EmptyTuple] {
      def apply[K, V](m: Map[K, V]): Option[EmptyTuple] = Some(EmptyTuple)
    }

  given tupleNFromMap[K0, V0, T <: Tuple](
    using k: ValueOf[K0],
    tv: TypeTest[Any, V0],
    fmt: FromMap[T],
  ): FromMap[(K0 ->> V0) *: T] =
    new FromMap[(K0 ->> V0) *: T] {
      def apply[K, V](m: Map[K, V]): Option[(K0 ->> V0) *: T] =
        for {
          v <- m.get(k.value.asInstanceOf[K])
          v0 <- tv.unapply(v)
          t <- fmt(m)
        } yield label[K0](v0) *: t
    }
}
