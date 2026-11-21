package formless.hlist

/**
 * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at an element of type ${U}: no element has that type, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait SplitLeft[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object SplitLeft {
  type Aux[L, U, P, S] = SplitLeft[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: SplitLeft[T, U]): SplitLeft.Aux[T, U, s.Prefix, s.Suffix] = s

  given splitLeftAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.SplitLeft]): SplitLeft.Aux[L, U, f.A, f.B] =
    new SplitLeft[L, U] {
      type Prefix = f.A
      type Suffix = f.B
      def apply(l: L): Out = HList.splitBefore(l, f.index).asInstanceOf[Out]
    }
}
