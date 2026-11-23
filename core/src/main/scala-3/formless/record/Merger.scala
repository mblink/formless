package formless.record

import formless.hlist.{::, DepFn2, HList, HNil}

/**
 * Type class supporting record merging.
 */
@scala.annotation.implicitNotFound("Cannot merge record ${L} with ${M}: a shared key may have different value types or the field lookup could be undecidable. Use updateWith to make the value types agree, remove the field first, or use mergeWith to combine the values.")
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
    using u: Updater[L, K ->> V] { type Out = U },
    mu: Merger[U, M],
  ): Merger.Aux[L, (K ->> V) :: M, mu.Out] =
    new Merger[L, (K ->> V) :: M] {
      type Out = mu.Out
      def apply(l: L, m: (K ->> V) :: M): Out =
        mu(u(l, m.head), m.tail)
    }
}
