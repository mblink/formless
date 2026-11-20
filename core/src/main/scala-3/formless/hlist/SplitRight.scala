package formless.hlist

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
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

  @deprecated("Retained for binary compatibility", "0.8.1")
  private[hlist] def splitRightHList[L <: HList, U](
    using f: FindField[HList.Reverse[L], U],
  ): SplitRight.Aux[L, U, HList.Append[HList.Reverse[f.Tail], U], HList.Reverse[f.Head]] =
    new SplitRight[L, U] {
      type Prefix = HList.Append[HList.Reverse[f.Tail], U]
      type Suffix = HList.Reverse[f.Head]
      def apply(l: L): Out = {
        val a = l.toArray.reverse
        (
          HList.fromArray(a.drop(f.index + 1).reverse :+ a(f.index)).asInstanceOf[Prefix],
          HList.fromArray(a.take(f.index).reverse).asInstanceOf[Suffix],
        )
      }
    }
}
