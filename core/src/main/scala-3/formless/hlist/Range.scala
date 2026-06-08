package formless.hlist

import compiletime.ops.int.S

@deprecated("Retained for binary compatibility", "0.7.0")
private[hlist] type RangeT[N <: Int, M <: Int] <: HList = N match {
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

  @deprecated("Retained for binary compatibility", "0.7.0")
  private[hlist] final class Inst[N, M, O](o: O) extends Range[N, M], Serializable {
    final type Out = O
    final def apply(): Out = o
  }

  given range1[A <: Int]: Range.Aux[A, A, HNil] =
    new Range[A, A] {
      type Out = HNil
      def apply(): Out = HNil
    }

  given range2[A <: Int, B <: Int, L <: HList, O <: HList](
    using b: ValueOf[B],
    r: Range.Aux[A, B, L],
    p: Prepend.Aux[L, B :: HNil, O],
  ): Aux[A, S[B], O] =
    new Range[A, S[B]] {
      type Out = O
      def apply(): Out = p(r(), b.value :: HNil)
    }
}
