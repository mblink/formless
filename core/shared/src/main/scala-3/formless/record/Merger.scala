package formless.record

import formless.hlist.{::, DepFn2, HList, HNil}

/**
 * Type class support record merging.
 */
trait Merger[L, M] extends DepFn2[L, M], Serializable

object Merger {
  type Aux[L, M, O] = Merger[L, M] { type Out = O }

  inline def apply[L, M](using m: Merger[L, M]): Merger.Aux[L, M, m.Out] = m

  given mergerHNilL[L]: Merger.Aux[L, HNil, L] =
    new Merger[L, HNil] {
      type Out = L
      def apply(l: L, m: HNil): Out = l
    }

  given mergerHNilR[M]: Merger.Aux[HNil, M, M] =
    new Merger[HNil, M] {
      type Out = M
      def apply(l: HNil, m: M): Out = m
    }

  given updateMerger[K <: Singleton, V, L, M <: HList, U](
    using u: Updater.Aux[L, K ->> V, U],
    mu: Merger[U, M],
  ): Merger.Aux[L, (K ->> V) :: M, mu.Out] =
    new Merger[L, (K ->> V) :: M] {
      type Out = mu.Out
      def apply(l: L, m: (K ->> V) :: M): Out =
        mu(u(l, m.head), m.tail)
    }
}
