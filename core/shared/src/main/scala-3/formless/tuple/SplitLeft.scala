package formless.tuple

/**
 * Type class supporting splitting this `Tuple` at the first occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `Tuple` contains an element of type `U`.
 */
trait SplitLeft[L, U] extends DepFn1[L] with Serializable {
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

  given tupleSplitLeft[L <: Tuple, U](using f: FindField[L, U]): SplitLeft.Aux[L, U, f.Head, U *: f.Tail] =
    new SplitLeft[L, U] {
      type Prefix = f.Head
      type Suffix = U *: f.Tail
      def apply(l: L): Out = (l.take(f.index).asInstanceOf[Prefix], l.drop(f.index).asInstanceOf[Suffix])
    }
}
