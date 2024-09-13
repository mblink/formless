package formless.hlist

type MapConsT[A, M <: HList] <: HList = M match {
  case HNil => HNil
  case h :: t => (A :: HListType[h]) :: MapConsT[A, t]
}

/**
 * Type class supporting consing an element onto each row of this `HList` of `HList`s.
 */
trait MapCons[A, M] extends DepFn2[A, M], Serializable

object MapCons {
  type Aux[A, M, O] = MapCons[A, M] { type Out = O }

  inline def apply[A, M](using m: MapCons[A, M]): MapCons.Aux[A, M, m.Out] = m

  given mapConsHList[A, M <: HList](
    using ev: LiftAll[[a] =>> a <:< HList, M],
  ): MapCons.Aux[A, M, MapConsT[A, M]] =
    new MapCons[A, M] {
      type Out = MapConsT[A, M]
      def apply(a: A, m: M): Out =
        HList.fromArray(m.toArray.map[HList](x =>
          HList.fromArray(a +: x.asInstanceOf[HList].toArray)
        )).asInstanceOf[Out]
    }
}
