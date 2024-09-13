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

  final class Inst[N, M, O](o: O) extends Range[N, M], Serializable {
    final type Out = O
    final def apply(): Out = o
  }

  inline given rangeInst[N <: Int, M <: Int](
    using ev: (M >= N) =:= true,
  ): Range.Aux[N, M, RangeT[N, M]] = Inst(summonAllValueOfHList[RangeT[N, M]])
}
