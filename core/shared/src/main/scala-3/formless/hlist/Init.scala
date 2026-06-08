package formless.hlist

/**
 * Type class supporting access to all but the last element of this `HList`. Available only if this `HList` has at least one element.
 */
trait Init[T] extends DepFn1[T], Serializable

object Init {
  type Aux[T, O] = Init[T] { type Out = O }

  inline def apply[T](using l: Init[T]): Init.Aux[T, l.Out] = l

  given lastNonEmptyhList[T <: NonEmptyHList]: Init.Aux[T, HList.Init[T]] =
    new Init[T] {
      type Out = HList.Init[T]
      def apply(t: T): Out = HList.init(t)
    }
}
