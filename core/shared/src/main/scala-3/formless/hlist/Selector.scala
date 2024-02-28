package formless.hlist

/**
 * Type class supporting access to the first element of type `U` in this `HList`. Available only if this `HList`
 * contains an element of type `U`.
 */
sealed trait Selector[T <: HList, A] extends (T => A) with Serializable

object Selector {
  inline def apply[T <: HList, A](using s: Selector[T, A]): Selector[T, A] = s

  inline given selectorHListHead[H, T <: HList]: Selector[H :: T, H] =
    new Selector[H :: T, H] { def apply(t: H :: T): H = t.head }

  inline given selectorHListTail[A, H, T <: HList](using s: Selector[T, A]): Selector[H :: T, A] =
    new Selector[H :: T, A] { def apply(t: H :: T): A = s(t.tail) }
}
