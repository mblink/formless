package formless.hlist

/**
 * Type class supporting left-reducing a polymorphic binary function over this `HList`.
 */
trait LeftReducer[L <: HList, F] extends DepFn1[L] with Serializable

object LeftReducer {
  type Aux[L <: HList, F, O] = LeftReducer[L, F] { type Out = O }

  inline def apply[L <: HList, F](using r: LeftReducer[L, F]): LeftReducer.Aux[L, F, r.Out] = r

  given leftReducerHList[H, T <: HList, F, O](using f: LeftFolder.Aux[T, H, F, O]): LeftReducer.Aux[H :: T, F, O] =
    new LeftReducer[H :: T, F] {
      type Out = O
      def apply(l: H :: T): Out = f(l.tail, l.head)
    }
}
