package formless.hlist

/**
 * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
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
      def apply(l: L): Out = (HList.take0(l, f.index), HList.drop0(l, f.index)).asInstanceOf[Out]
    }
}
