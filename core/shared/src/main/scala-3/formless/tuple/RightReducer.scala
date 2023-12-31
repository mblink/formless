package formless.tuple

/**
 * Type class supporting right-reducing a polymorphic binary function over this `Tuple`.
 */
trait RightReducer[L <: Tuple, F] extends DepFn1[L] with Serializable

object RightReducer {
  type Aux[L <: Tuple, F, O] = RightReducer[L, F] { type Out = O }

  inline def apply[L <: Tuple, F](using r: RightReducer[L, F]): RightReducer.Aux[L, F, r.Out] = r

  given rightReducerTuple1[H, F]: RightReducer.Aux[H *: EmptyTuple, F, H] =
    new RightReducer[H *: EmptyTuple, F] {
      type Out = H
      def apply(l: H *: EmptyTuple): Out = l.head
    }

  given rightReducerTupleN[H, T <: Tuple, F, OutT, Out0](
    using rt: RightReducer.Aux[T, F, OutT],
    f: Case2.Aux[F, H, OutT, Out0],
  ): RightReducer.Aux[H *: T, F, Out0] =
      new RightReducer[H *: T, F] {
        type Out = Out0
        def apply(l: H *: T): Out = f(l.head, rt(l.tail))
      }
}
