package formless.hlist

type ConstMapperT[C, T <: HList] <: HList = T match {
  case HNil => HNil
  case _ :: t => C :: ConstMapperT[C, t]
}

/**
 * Type class supporting mapping a constant valued function over this `HList`.
 */
trait ConstMapper[C, T <: HList] extends DepFn2[C, T] with Serializable { type Out <: HList }

object ConstMapper {
  type Aux[C, T <: HList, O <: HList] = ConstMapper[C, T] { type Out = O }

  inline def apply[C, T <: HList](using m: ConstMapper[C, T]): ConstMapper.Aux[C, T, m.Out] = m

  private def constMap[C, T <: HList](c: C, t: T): ConstMapperT[C, T] =
    t match {
      case _: HNil => HNil
      case x: (_ :: t) => c :: constMap[C, t](c, x.tail)
    }

  given constMapperHList[C, T <: HList]: ConstMapper.Aux[C, T, ConstMapperT[C, T]] =
    new ConstMapper[C, T] {
      type Out = ConstMapperT[C, T]
      def apply(c: C, t: T): Out = constMap(c, t)
    }
}
