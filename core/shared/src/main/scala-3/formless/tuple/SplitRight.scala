package formless.tuple

/**
 * Type class supporting splitting this `Tuple` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `Tuple` contains an element of type `U`.
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

  given tupleSplitRight[L <: Tuple, U](
    using f: FindField[ReverseT[L], U],
  ): SplitRight.Aux[L, U, Tuple.Append[ReverseT[f.Tail], U], ReverseT[f.Head]] =
    new SplitRight[L, U] {
      type Prefix = Tuple.Append[ReverseT[f.Tail], U]
      type Suffix = ReverseT[f.Head]
      def apply(l: L): Out = {
        val a = l.toArray.reverse
        (
          Tuple.fromArray(a.drop(f.index + 1).reverse :+ a(f.index)).asInstanceOf[Prefix],
          Tuple.fromArray(a.take(f.index).reverse).asInstanceOf[Suffix],
        )
      }
    }
}
