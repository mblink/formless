package typify.tuple

trait RightFolder[L <: Tuple, In, F] extends DepFn2[L, In]

object RightFolder {
  inline def apply[L <: Tuple, In, F](using f: RightFolder[L, In, F]): Aux[L, In, F, f.Out] = f

  type Aux[L <: Tuple, In, F, Out0] = RightFolder[L, In, F] { type Out = Out0 }

  implicit def hnilRightFolder[In, F]: Aux[EmptyTuple, In, F, In] =
    new RightFolder[EmptyTuple, In, F] {
      type Out = In
      def apply(l: EmptyTuple, in: In): Out = in
    }

  implicit def hlistRightFolder[H, T <: Tuple, In, F, OutT, FOut](
    using ft: RightFolder.Aux[T, In, F, OutT],
    f: Case2[F, H, OutT, FOut],
  ): Aux[H *: T, In, F, FOut] =
    new RightFolder[H *: T, In, F] {
      type Out = FOut
      def apply(l: H *: T, in: In): Out = f.run(l.head, ft(l.tail, in))
    }
}
