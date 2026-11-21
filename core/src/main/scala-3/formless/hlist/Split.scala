package formless.hlist

/**
 * Type class supporting splitting this `HList` into a prefix of length `N` and the remaining suffix.
 * The count must be between zero and the length of the list, inclusive.
 */
trait Split[L, N] extends DepFn1[L], Serializable {
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

  given splitHList[L <: HList, N <: Int](
    using ev: HList.CountInBounds[L, N],
    nv: ValueOf[N],
  ): Split.Aux[L, N, HList.Take[L, N], HList.Drop[L, N]] =
    new Split[L, N] {
      type Prefix = HList.Take[L, N]
      type Suffix = HList.Drop[L, N]
      private lazy val n = nv.value
      def apply(l: L): Out = (HList.take(l, n), HList.drop(l, n))
    }
}
