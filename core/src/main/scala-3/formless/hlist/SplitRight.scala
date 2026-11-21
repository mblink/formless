package formless.hlist

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at its last element of type ${U}: no element has that type, or a later one could be in an abstract part of the type (a type parameter or abstract type member)")
trait SplitRight[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object SplitRight {
  type Aux[L, U, P, S] = SplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: SplitRight[T, U]): SplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  given splitRightAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.SplitRight]): SplitRight.Aux[L, U, f.A, f.B] =
    new SplitRight[L, U] {
      type Prefix = f.A
      type Suffix = f.B
      def apply(l: L): Out = (HList.take0(l, f.index + 1), HList.drop0(l, f.index + 1)).asInstanceOf[Out]
    }
}
