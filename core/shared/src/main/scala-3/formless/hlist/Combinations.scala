package formless.hlist

import compiletime.ops.int.S

/**
 * Type class supporting the calculation of every combination of this `HList`
 */
trait Combinations[N, L] extends DepFn1[L], Serializable

trait CombinationsLP {
  final type Aux[N, L, O] = Combinations[N, L] { type Out = O }

  final given combinationsHNil[N]: Combinations.Aux[N, HNil, HNil] =
    new Combinations[N, HNil] {
      type Out = HNil
      def apply(l: HNil): Out = HNil
    }
}

object Combinations extends CombinationsLP {
  inline def apply[N, L](using c: Combinations[N, L]): Combinations.Aux[N, L, c.Out] = c
  inline def apply[N, L](n: N, l: L)(using c: Combinations[N, L]): c.Out = c(l)

  given combination0[L]: Combinations.Aux[0, L, HNil :: HNil] =
    new Combinations[0, L] {
      type Out = HNil :: HNil
      def apply(l: L): Out = HNil :: HNil
    }

  given combinationN[N <: Int, H, T <: HList, C1 <: HList, C2 <: HList, CM <: HList, CpOut <: HList](
    using c1: Combinations.Aux[N, T, C1],
    c2: Combinations.Aux[S[N], T, C2],
    cm: MapCons.Aux[H, C1, CM],
    cp: Prepend.Aux[CM, C2, CpOut],
  ): Combinations.Aux[S[N], H :: T, CpOut] =
    new Combinations[S[N], H :: T] {
      type Out = cp.Out
      def apply(l: H :: T): Out = cp(cm(l.head, c1(l.tail)), c2(l.tail))
    }
}
