package formless.hlist

/**
 * Type class supporting reifying an `HList` of singleton types.
 */
trait Reify[T] extends DepFn0, Serializable { type Out }

object Reify {
  inline def apply[T](using r: Reify[T]): Reify.Aux[T, r.Out] = r

  type Aux[T, O] = Reify[T] { type Out = O }

  inline given reifyInst[T <: HList]: Reify.Aux[T, T] =
    new Reify[T] {
      type Out = T
      def apply(): T = summonAllValueOfHList[T]
    }
}
