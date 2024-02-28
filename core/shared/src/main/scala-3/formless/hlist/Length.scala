package formless.hlist

/**
 * Type class supporting computing the singleton `Int` corresponding to the length of this `HList`.
 */
trait Length[T] extends DepFn0 with Serializable { type Out <: Int }

object Length {
  type Aux[T, O <: Int] = Length[T] { type Out = O }

  inline def apply[T](using l: Length[T]): Length.Aux[T, l.Out] = l

  inline given lengthHList[T <: HList]: Length.Aux[T, HList.Size[T]] =
    new Length[T] {
      type Out = HList.Size[T]
      def apply(): Out = compiletime.summonInline[ValueOf[HList.Size[T]]].value
    }
}
