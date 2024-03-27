package formless.record

import formless.hlist.{::, DepFn1, HList, HNil}

/**
 * Type class supporting multiple record field selection.
 */
trait SelectAll[L, K] extends DepFn1[L] with Serializable

object SelectAll {
  type Aux[L, K, O] = SelectAll[L, K] { type Out = O }

  inline def apply[L, K](using s: SelectAll[L, K]): SelectAll.Aux[L, K, s.Out] = s

  given selectAllHNil[L]: SelectAll.Aux[L, HNil, HNil] =
    new SelectAll[L, HNil] {
      type Out = HNil
      def apply(l: L): Out = HNil
    }

  given selectAllHCons[L <: HList, KH, KT <: HList](
    using sh: Selector[L, KH],
    st: SelectAll[L, KT] { type Out <: HList },
  ): SelectAll.Aux[L, KH :: KT, sh.Out :: st.Out] =
    new SelectAll[L, KH :: KT] {
      type Out = sh.Out :: st.Out
      def apply(l: L): Out = sh(l) :: st(l)
    }
}
