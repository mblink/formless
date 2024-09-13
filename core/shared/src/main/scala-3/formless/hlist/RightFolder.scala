package formless.hlist

/**
 * Type class supporting right-folding a polymorphic binary function over this `HList`.
 */
trait RightFolder[L <: HList, In, F] extends DepFn2[L, In], Serializable

object RightFolder {
  type Aux[L <: HList, In, F, O] = RightFolder[L, In, F] { type Out = O }

  inline def apply[L <: HList, In, F](using f: RightFolder[L, In, F]): RightFolder.Aux[L, In, F, f.Out] = f

  given rightFolderHNil[In, F]: RightFolder.Aux[HNil, In, F, In] =
    new RightFolder[HNil, In, F] {
      type Out = In
      def apply(l: HNil, in: In): Out = in
    }

  given rightFolderHCons[H, T <: HList, In, F, OutT, FOut](
    using ft: RightFolder.Aux[T, In, F, OutT],
    f: Case2.Aux[F, H, OutT, FOut],
  ): RightFolder.Aux[H :: T, In, F, FOut] =
    new RightFolder[H :: T, In, F] {
      type Out = FOut
      def apply(l: H :: T, in: In): Out = f(l.head, ft(l.tail, in))
    }
}
