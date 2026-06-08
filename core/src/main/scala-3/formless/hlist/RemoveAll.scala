package formless.hlist

/**
 * Type class supporting removal of a sublist from this `HList`. Available only if this `HList` contains a
 * sublist of type `SL`.
 *
 * The elements of `SL` do not have to be contiguous in this `HList`.
 */
trait RemoveAll[L, SL] extends DepFn1[L], Serializable {
  def reinsert(out: Out): L
}

object RemoveAll {
  type Aux[L, SL, O] = RemoveAll[L, SL] { type Out = O }

  inline def apply[L, SL](using r: RemoveAll[L, SL]): RemoveAll.Aux[L, SL, r.Out] = r

  given removeAllHNil[L]: RemoveAll.Aux[L, HNil, (HNil, L)] =
    new RemoveAll[L, HNil] {
      type Out = (HNil, L)
      def apply(l : L): Out = (HNil, l)
      def reinsert(out: (HNil, L)): L = out._2
    }

  given removeAllHCons[L <: HList, E, RemE <: HList, Rem <: HList, SLT <: HList](
    using rt: Remove.Aux[L, E, (E, RemE)],
    st: RemoveAll.Aux[RemE, SLT, (SLT, Rem)],
  ): RemoveAll.Aux[L, E :: SLT, (E :: SLT, Rem)] =
    new RemoveAll[L, E :: SLT] {
      type Out = (E :: SLT, Rem)
      def apply(l : L): Out = {
        val (e, rem) = rt(l)
        val (sl, left) = st(rem)
        (e :: sl, left)
      }

      def reinsert(out: (E :: SLT, Rem)): L =
        rt.reinsert((out._1.head, st.reinsert((out._1.tail, out._2))))
    }
}
