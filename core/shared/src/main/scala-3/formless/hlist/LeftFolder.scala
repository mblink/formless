package formless.hlist

/**
 * Type class supporting left-folding a polymorphic binary function over this `HList`.
 */
trait LeftFolder[L <: HList, In, F] extends DepFn2[L, In], Serializable

object LeftFolder {
  type Aux[L <: HList, In, F, O] = LeftFolder[L, In, F] { type Out = O }

  inline def apply[L <: HList, In, F](using f: LeftFolder[L, In, F]): LeftFolder.Aux[L, In, F, f.Out] = f

  given leftFolderHNil[In, F]: LeftFolder.Aux[HNil, In, F, In] =
    new LeftFolder[HNil, In, F] {
      type Out = In
      def apply(l: HNil, in: In): Out = in
    }

  given leftFolderHCons[H, T <: HList, In, F, OutH, FtOut](
    using f: Case2.Aux[F, In, H, OutH],
    ft: LeftFolder.Aux[T, OutH, F, FtOut],
  ): LeftFolder.Aux[H :: T, In, F, FtOut] =
    new LeftFolder[H :: T, In, F] {
      type Out = FtOut
      def apply(l: H :: T, in: In): Out = ft(l.tail, f(in, l.head))
    }
}
