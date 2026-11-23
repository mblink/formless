package formless.record

import formless.hlist.{::, DepFn2, HList, HNil}
import shapeless.<:!<
import shapeless.labelled.KeyTag
import shapeless.ops.hlist.Prepend

/** Type class supporting record merging. */
@scala.annotation.implicitNotFound("Cannot merge record ${L} with ${M}: a shared key may have different value types or the field lookup could be undecidable. Use updateWith to make the value types agree, remove the field first, or use mergeWith to combine the values.")
trait Merger[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable {
  type Out <: HList
}

private[record] trait MergerLowPriority {
  implicit def mergerHNilL[L <: HList, M <: HNil]: Merger.Aux[L, M, L] =
    new Merger[L, M] {
      type Out = L
      def apply(l: L, m: M): Out = l
    }

  implicit def updateMerger[K, V, L <: HList, M <: HList, U <: HList, O <: HList](
    implicit u: Updater[L, K ->> V] { type Out = U },
    mu: Merger[U, M] { type Out = O },
  ): Merger.Aux[L, (K ->> V) :: M, O] =
    new Merger[L, (K ->> V) :: M] {
      type Out = O
      def apply(l: L, m: (K ->> V) :: M): Out = mu(u(l, m.head), m.tail)
    }

  // Scala 2 also supports merging HLists with unlabelled elements, which are appended even when their types repeat.
  implicit def appendUnlabelled[F, L <: HList, M <: HList, U <: HList, O <: HList](
    implicit @annotation.unused unlabelled: F <:!< KeyTag[_, _],
    @annotation.unused safe: Updater[L, F],
    prepend: Prepend.Aux[L, F :: HNil, U],
    mu: Merger[U, M] { type Out = O },
  ): Merger.Aux[L, F :: M, O] =
    new Merger[L, F :: M] {
      type Out = O
      def apply(l: L, m: F :: M): Out = mu(prepend(l, m.head :: HNil), m.tail)
    }
}

object Merger extends MergerLowPriority {
  type Aux[L <: HList, M <: HList, O <: HList] = Merger[L, M] { type Out = O }

  def apply[L <: HList, M <: HList](implicit m: Merger[L, M]): Aux[L, M, m.Out] = m

  implicit def mergerHNilR[M <: HList]: Aux[HNil, M, M] =
    new Merger[HNil, M] {
      type Out = M
      def apply(l: HNil, m: M): Out = m
    }
}
