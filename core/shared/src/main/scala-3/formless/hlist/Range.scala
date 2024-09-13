package formless.hlist

import compiletime.ops.int.{>=, S}

type RangeT[N <: Int, M <: Int] <: HList = N match {
  case M => HNil
  case _ => N :: RangeT[S[N], M]
}

/**
 * Type class supporting the patching of a `HList`.
 */
trait Range[N, M] extends DepFn0, Serializable

object Range {
  type Aux[N, M, O] = Range[N, M] { type Out = O }

  inline def apply[N, M](using r: Range[N, M]): Range.Aux[N, M, r.Out] = r

  inline given rangeInst[N <: Int, M <: Int](
    using ev: (M >= N) =:= true,
  ): Range.Aux[N, M, RangeT[N, M]] =
    new Range[N, M] {
      type Out = RangeT[N, M]
      def apply(): Out = summonAllValueOfHList[RangeT[N, M]]
    }
}
