package formless.hlist

/**
 * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at an element of type ${U}: no element has that type, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait ReverseSplitLeft[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object ReverseSplitLeft {
  type Aux[L, U, P, S] = ReverseSplitLeft[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: ReverseSplitLeft[T, U]): ReverseSplitLeft.Aux[T, U, s.Prefix, s.Suffix] = s

  given reverseSplitLeftAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.ReverseSplitLeft]): ReverseSplitLeft.Aux[L, U, f.A, f.B] =
    new ReverseSplitLeft[L, U] {
      type Prefix = f.A
      type Suffix = f.B
      def apply(l: L): Out = HList.reverseSplitBefore(l, f.index).asInstanceOf[Out]
    }
}
