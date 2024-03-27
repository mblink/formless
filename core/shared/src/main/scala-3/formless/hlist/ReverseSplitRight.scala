package formless.hlist

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
trait ReverseSplitRight[L, U] extends DepFn1[L] with Serializable {
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

  given reverseSplitRightHList[L <: HList, U](
    using f: FindField[HList.Reverse[L], U],
  ): ReverseSplitRight.Aux[L, U, U :: f.Tail, HList.Reverse[f.Head]] =
    new ReverseSplitRight[L, U] {
      type Prefix = U :: f.Tail
      type Suffix = HList.Reverse[f.Head]
      def apply(l: L): Out = {
        val a = l.toArray.reverse
        (
          HList.fromArray(a(f.index) +: a.drop(f.index + 1)).asInstanceOf[Prefix],
          HList.fromArray(a.take(f.index).reverse).asInstanceOf[Suffix]
        )
      }
    }
}
