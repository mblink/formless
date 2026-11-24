package formless.hlist

/**
 * Type class supporting splitting this `HList` into a reversed prefix of length `N` and the remaining suffix.
 * The count must be between zero and the length of the list, inclusive.
 */
trait ReverseSplit[L, N] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object ReverseSplit {
  type Aux[L, N, P, S] = ReverseSplit[L, N] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, N](using s: ReverseSplit[T, N]): ReverseSplit.Aux[T, N, s.Prefix, s.Suffix] = s

  given splitHList[L <: HList, N <: Int](
    using ev: HList.CountInBounds[L, N],
    nv: ValueOf[N],
  ): ReverseSplit.Aux[L, N, HList.Reverse[HList.Take[L, N]], HList.Drop[L, N]] =
    new ReverseSplit[L, N] {
      type Prefix = HList.Reverse[HList.Take[L, N]]
      type Suffix = HList.Drop[L, N]
      private lazy val n = nv.value
      def apply(l: L): Out = (HList.fromArray(HList.take(l, n).toArray.reverse).asInstanceOf[Prefix], HList.drop(l, n).asInstanceOf[Suffix])
    }
}
