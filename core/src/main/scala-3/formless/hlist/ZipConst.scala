package formless.hlist

type ZipConstT[C, L <: HList] <: HList = L match {
  case HNil => HNil
  case h :: t => (h, C) :: ZipConstT[C, t]
}

/**
 * Type class supporting zipping a `HList` with a constant, resulting in a `HList` of `HList2`s of the form
 * ({element from input `HList`}, {supplied constant})
 */
trait ZipConst[C, L] extends DepFn2[C, L], Serializable

object ZipConst {
  type Aux[C, L, O] = ZipConst[C, L] { type Out = O }

  inline def apply[C, L](using z: ZipConst[C, L]): ZipConst.Aux[C, L, z.Out] = z

  private def zipConst[C, T <: HList](c: C, t: T): ZipConstT[C, T] =
    t match {
      case _: HNil => HNil
      case x: (h :: t) => (x.head.asInstanceOf[h], c) :: zipConst[C, t](c, x.tail.asInstanceOf[t])
    }

  given zipConstHList[C, L <: HList]: ZipConst.Aux[C, L, ZipConstT[C, L]] =
    new ZipConst[C, L] {
      type Out = ZipConstT[C, L]
      def apply(c: C, l: L): Out = zipConst[C, L](c, l)
    }
}
