package formless.hlist

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at its last element of type ${U}: no element has that type, a later one could be in an abstract part of the type (a type parameter or abstract type member), or it is at different indexes in the arms of a union")
trait ReverseSplitRight[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object ReverseSplitRight {
  type Aux[L, U, P, S] = ReverseSplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: ReverseSplitRight[T, U]): ReverseSplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  given reverseSplitRightAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.ReverseSplitRight]): ReverseSplitRight.Aux[L, U, f.A, f.B] =
    new ReverseSplitRight[L, U] {
      type Prefix = f.A
      type Suffix = f.B
      def apply(l: L): Out = HList.reverseSplitAfter(l, f.index).asInstanceOf[Out]
    }
}
