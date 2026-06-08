package formless.hlist

/**
 * Type class supporting computing the singleton `Int` corresponding to the length of this `HList`.
 */
trait Length[T] extends DepFn0, Serializable { type Out <: Int }

object Length {
  type Aux[T, O <: Int] = Length[T] { type Out = O }

  inline def apply[T](using l: Length[T]): Length.Aux[T, l.Out] = l

  final class Inst[T, O <: Int](o: O) extends Length[T], Serializable {
    final type Out = O
    final def apply(): Out = o
  }

  inline given lengthHList[T <: HList]: Length.Aux[T, HList.Size[T]] =
    Inst(compiletime.summonInline[ValueOf[HList.Size[T]]].value)
}
