package formless.record

import formless.hlist.{::, DepFn1, HList, HNil}

/**
 * Type class supporting removal and re-insertion of a `HList` of elements (possibly unlabelled).
 */
trait RemoveAll[L, A] extends DepFn1[L], Serializable {
  def reinsert(out: Out): L
}

object RemoveAll {
  type Aux[L, A, O] = RemoveAll[L, A] { type Out = O }

  inline def apply[L, A](using r: RemoveAll[L, A]): RemoveAll.Aux[L, A, r.Out] = r

  given removeAllHNil[L]: RemoveAll.Aux[L, HNil, (HNil, L)] =
    new RemoveAll[L, HNil] {
      type Out = (HNil, L)
      def apply(l: L): Out = (HNil, l)
      def reinsert(out: Out): L = out._2
    }

  given removeAllHCons[L <: HList, H, T <: HList, OutT <: HList, RemovedH, RemainderH <: HList, RemovedT <: HList, RemainderT <: HList](
    using rt: RemoveAll.Aux[L, T, (RemovedT, RemainderT)],
    rh: Remove.Aux[RemainderT, H, (RemovedH, RemainderH)],
  ): RemoveAll.Aux[L, H :: T, (RemovedH :: RemovedT, RemainderH)] =
      new RemoveAll[L, H :: T] {
        type Out = (RemovedH :: RemovedT, RemainderH)
        def apply(l: L): Out = {
          val (removedT, remainderT) = rt(l)
          val (removedH, remainderH) = rh(remainderT)
          (removedH :: removedT, remainderH)
        }
        def reinsert(out: Out): L = rt.reinsert((out._1.tail, rh.reinsert((out._1.head, out._2))))
      }
}
