package typify.tuple

sealed trait Selector[T <: Tuple, A] extends (T => A)

object Selector {
  implicit inline def head[H, T <: Tuple]: Selector[H *: T, H] =
    new Selector[H *: T, H] { def apply(t: H *: T): H = t.head }

  implicit inline def tail[A, H, T <: Tuple](using s: Selector[T, A]): Selector[H *: T, A] =
    new Selector[H *: T, A] { def apply(t: H *: T): A = s(t.tail) }
}
