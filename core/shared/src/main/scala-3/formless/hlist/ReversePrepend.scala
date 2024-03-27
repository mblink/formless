package formless.hlist

/**
 * Type class supporting prepending `L` in reverse order to `R`.
 */
trait ReversePrepend[L, R] extends DepFn2[L, R] with Serializable

object ReversePrepend {
  type Aux[L, R, O] = ReversePrepend[L, R] { type Out = O }

  inline def apply[L, R](using p: ReversePrepend[L, R]): ReversePrepend.Aux[L, R, p.Out] = p

  given reversePrependHList[L <: HList, R <: HList]: ReversePrepend.Aux[L, R, HList.Concat[HList.Reverse[L], R]] =
    new ReversePrepend[L, R] {
      type Out = HList.Concat[HList.Reverse[L], R]
      def apply(l: L, r: R): Out = HList.fromArray(l.toArray.reverse ++ r.toArray).asInstanceOf[Out]
    }
}
