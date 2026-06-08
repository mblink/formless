package formless.hlist

/**
 * Type class supporting right-reducing a polymorphic binary function over this `HList`.
 */
trait RightReducer[L <: HList, F] extends DepFn1[L], Serializable

object RightReducer {
  type Aux[L <: HList, F, O] = RightReducer[L, F] { type Out = O }

  inline def apply[L <: HList, F](using r: RightReducer[L, F]): RightReducer.Aux[L, F, r.Out] = r

  given rightReducerHCons1[H, F]: RightReducer.Aux[H :: HNil, F, H] =
    new RightReducer[H :: HNil, F] {
      type Out = H
      def apply(l: H :: HNil): Out = l.head
    }

  given rightReducerHConsN[H, T <: HList, F, OutT, Out0](
    using rt: RightReducer.Aux[T, F, OutT],
    f: Case2.Aux[F, H, OutT, Out0],
  ): RightReducer.Aux[H :: T, F, Out0] =
      new RightReducer[H :: T, F] {
        type Out = Out0
        def apply(l: H :: T): Out = f(l.head, rt(l.tail))
      }
}
