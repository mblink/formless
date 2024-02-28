package formless.hlist

/**
 * Type class supporting mapping a polymorphic function over this `HList` and then folding the result using a
 * monomorphic function value.
 */
trait MapFolder[L <: HList, R, F] extends Serializable {
  def apply(l: L, in: R, op: (R, R) => R): R
}

object MapFolder {
  inline def apply[L <: HList, R, F](using f: MapFolder[L, R, F]): MapFolder[L, R, F] = f

  given mapFolderHNil[R, F]: MapFolder[HNil, R, F] =
    new MapFolder[HNil, R, F] {
      def apply(l: HNil, in: R, op: (R, R) => R): R = in
    }

  given mapFolderHCons[H, T <: HList, R, F <: Poly](
    using hc: Case1.Aux[F, H, R],
    tf: MapFolder[T, R, F],
  ): MapFolder[H :: T, R, F] =
    new MapFolder[H :: T, R, F] {
      def apply(l: H :: T, in: R, op: (R, R) => R): R = op(hc(l.head), tf(l.tail, in, op))
    }
}
