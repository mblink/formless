package formless.hlist

/**
 * Type class supporting reversing this `HList`.
 */
trait Reverse[T] extends DepFn1[T], Serializable

object Reverse {
  type Aux[T, O] = Reverse[T] { type Out = O }

  inline def apply[T](using r: Reverse[T]): Reverse.Aux[T, r.Out] = r

  given reverseHList[T <: HList]: Reverse.Aux[T, HList.Reverse[T]] =
    new Reverse[T] {
      type Out = HList.Reverse[T]
      def apply(t: T): Out = HList.fromArray(t.toArray.reverse).asInstanceOf[Out]
    }
}
