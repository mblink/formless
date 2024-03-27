package formless.hlist

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
trait SplitRight[L, U] extends DepFn1[L] with Serializable {
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

  given splitRightHList[L <: HList, U](
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
