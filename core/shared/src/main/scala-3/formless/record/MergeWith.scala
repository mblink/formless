package formless.record

import formless.hlist.{::, Case2, DepFn2, HList, HNil}

/**
 * Type class support record merging with a polymorphic function.
 */
trait MergeWith[L, M, F] extends DepFn2[L, M], Serializable

sealed trait MergeWithLP {
  final type Aux[L, M, F, O] = MergeWith[L, M, F] { type Out = O }

  final given mergeWithHCons1[K, V, T <: HList, M <: HList, F](
    using mt: MergeWith[T, M, F] { type Out <: HList },
    lk: LacksKey[M, K]
  ): MergeWith.Aux[(K ->> V) :: T, M, F, (K ->> V) :: mt.Out] =
    new MergeWith[(K ->> V) :: T, M, F] {
      type Out = (K ->> V) :: mt.Out
      def apply(l: (K ->> V) :: T, m: M): Out = l.head :: mt(l.tail, m)
    }
}

object MergeWith extends MergeWithLP {
  inline def apply[L, M, F](using m: MergeWith[L, M, F]): MergeWith.Aux[L, M, F, m.Out] = m

  given mergeWithHNil[M, F]: MergeWith.Aux[HNil, M, F, M] =
    new MergeWith[HNil, M, F] {
      type Out = M
      def apply(l: HNil, m: M): Out = m
    }

  given mergeWithHCons2[K, V0, V1, V, T <: HList, M <: HList, MT <: HList, F, Out0 <: HList](
    using rm: Remover.Aux[M, K, (V1, MT)],
    mt: MergeWith.Aux[T, MT, F, Out0],
    callback: Case2.Aux[F, V0, V1, V],
  ): MergeWith.Aux[(K ->> V0) :: T, M, F, (K ->> V) :: Out0] = {
    new MergeWith[(K ->> V0) :: T, M, F] {
      type Out = (K ->> V) :: mt.Out
      def apply(l: (K ->> V0) :: T, m: M): Out = {
        val (mv, mr) = rm(m)
        val up = label[K](callback(l.head: V0, mv))
        up :: mt(l.tail, mr)
      }
    }
  }
}
