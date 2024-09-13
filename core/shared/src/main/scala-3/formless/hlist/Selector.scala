package formless.hlist

/**
 * Type class supporting access to the first element of type `U` in this `HList`. Available only if this `HList`
 * contains an element of type `U`.
 */
sealed trait Selector[T <: HList, A] extends (T => A), Serializable

object Selector {
  inline def apply[T <: HList, A](using s: Selector[T, A]): Selector[T, A] = s

  final class Inst[T <: HList, A](f: T => A) extends Selector[T, A], Serializable {
    final def apply(t: T): A = f(t)
  }

  inline given selectorHListHead[H, T <: HList]: Selector[H :: T, H] = Inst(_.head)

  inline given selectorHListTail[A, H, T <: HList](using s: Selector[T, A]): Selector[H :: T, A] = Inst(t => s(t.tail))
}
