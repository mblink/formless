package formless.hlist

/**
 * Type class supporting prepending `L` to `R`.
 */
trait Prepend[L, R] extends DepFn2[L, R] with Serializable

object Prepend {
  type Aux[L, R, O] = Prepend[L, R] { type Out = O }

  inline def apply[L, R](using p: Prepend[L, R]): Prepend.Aux[L, R, p.Out] = p

  given prependHList[L <: HList, R <: HList]: Prepend.Aux[L, R, HList.Concat[L, R]] =
    new Prepend[L, R] {
      type Out = HList.Concat[L, R]
      def apply(l: L, r: R): Out = HList.concat(l, r)
    }
}
