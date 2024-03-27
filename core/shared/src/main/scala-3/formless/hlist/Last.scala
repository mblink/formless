package formless.hlist

/**
 * Type class supporting access to the last element of this `HList`. Available only if this `HList` has at least one element.
 */
trait Last[T] extends DepFn1[T] with Serializable

object Last {
  type Aux[T, O] = Last[T] { type Out = O }

  inline def apply[T](using l: Last[T]): Last.Aux[T, l.Out] = l

  given lastNonEmptyHList[T <: NonEmptyHList]: Last.Aux[T, HList.Last[T]] =
    new Last[T] {
      type Out = HList.Last[T]
      def apply(t: T): Out = HList.last(t)
    }
}
