package formless.tuple

import compiletime.ops.int.>=

/**
 * Type class supporting splitting this `Tuple` at the `N`th element returning the prefix and suffix as a pair.
 * Available only if this `Tuple` has at least `N` elements.
 */
trait Split[L, N] extends DepFn1[L] with Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object Split {
  type Aux[L, N, P, S] = Split[L, N] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, N](using s: Split[T, N]): Split.Aux[T, N, s.Prefix, s.Suffix] = s

  given splitTuple[L <: Tuple, N <: Int](
    using ev: (Tuple.Size[L] >= N) =:= true,
    nv: ValueOf[N],
  ): Split.Aux[L, N, Tuple.Take[L, N], Tuple.Drop[L, N]] =
    new Split[L, N] {
      type Prefix = Tuple.Take[L, N]
      type Suffix = Tuple.Drop[L, N]
      private lazy val n = nv.value
      def apply(l: L): Out = (l.take(n).asInstanceOf[Prefix], l.drop(n).asInstanceOf[Suffix])
    }
}
