package formless.hlist

type ZipWithIndexT[L <: HList] = ZipWithIndexT0[L, 0]

type ZipWithIndexT0[L <: HList, I <: Int] <: HList = L match {
  case HNil => HNil
  case h :: t => (h, I) :: ZipWithIndexT0[t, compiletime.ops.int.S[I]]
}

/**
 * Type class supporting zipping a `HList` with its element indices, resulting in a `HList` of `HList`s of the form
 * ({element from input `HList`}, {element index})
 */
trait ZipWithIndex[L] extends DepFn1[L] with Serializable

object ZipWithIndex {
  type Aux[L, O] = ZipWithIndex[L] { type Out = O }

  inline def apply[L](using z: ZipWithIndex[L]): ZipWithIndex.Aux[L, z.Out] = z

  given zipWithIndexHList[L <: HList]: ZipWithIndex.Aux[L, ZipWithIndexT[L]] =
    new ZipWithIndex[L] {
      type Out = ZipWithIndexT[L]
      def apply(l: L): Out = HList.fromArray(l.toArray.zipWithIndex).asInstanceOf[Out]
    }
}
