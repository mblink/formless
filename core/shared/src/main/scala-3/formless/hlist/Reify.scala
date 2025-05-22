package formless.hlist

/**
 * Type class supporting reifying an `HList` of singleton types.
 */
trait Reify[T] extends DepFn0, Serializable { type Out }

object Reify {
  inline def apply[T](using r: Reify[T]): Reify.Aux[T, r.Out] = r

  type Aux[T, O] = Reify[T] { type Out = O }

  @deprecated("Retained for binary compatibility", "0.7.0")
  private[hlist] final class Inst[T](t: T) extends Reify[T], Serializable {
    final type Out = T
    final def apply(): Out = t
  }

  given hnil: Reify.Aux[HNil, HNil] =
    new Reify[HNil] {
      type Out = HNil
      def apply(): Out = HNil
    }

  given hcons[H, T <: HList](using h: ValueOf[H], t: Reify.Aux[T, T]): Reify.Aux[H :: T, H :: T] =
    new Reify[H :: T] {
      type Out = H :: T
      def apply(): Out = h.value :: t()
    }
}
