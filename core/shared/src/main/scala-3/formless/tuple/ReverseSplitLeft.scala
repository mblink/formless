package formless.tuple

/**
 * Type class supporting splitting this `Tuple` at the first occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `Tuple` contains an element of type `U`.
 */
trait ReverseSplitLeft[L, U] extends DepFn1[L] with Serializable {
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

  given tupleReverseSplitLeft[L <: Tuple, U](
    using f: FindField[L, U],
  ): ReverseSplitLeft.Aux[L, U, ReverseT[f.Head], U *: f.Tail] =
    new ReverseSplitLeft[L, U] {
      type Prefix = ReverseT[f.Head]
      type Suffix = U *: f.Tail
      def apply(l: L): Out =
        (Tuple.fromArray(l.take(f.index).toArray.reverse).asInstanceOf[Prefix], l.drop(f.index).asInstanceOf[Suffix])
    }
}
