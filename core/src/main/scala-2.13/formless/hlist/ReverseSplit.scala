package formless
package hlist

/**
 * Type class supporting splitting this `HList` at the `N`th element returning the reverse prefix and suffix as a pair.
 * Available only if this `HList` has at least `N` elements.
 */
trait ReverseSplit[L, N] extends DepFn1[L] with Serializable {
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

  def apply[T, N](implicit s: ReverseSplit[T, N]): ReverseSplit.Aux[T, N, s.Prefix, s.Suffix] = s

  implicit def splitHList[L <: HList, I <: Int, N <: shapeless.Nat, P <: HList, S <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    ss: shapeless.ops.hlist.ReverseSplit.Aux[L, N, P, S],
  ): ReverseSplit.Aux[L, I, P, S] =
    new ReverseSplit[L, I] {
      type Prefix = P
      type Suffix = S
      def apply(l: L): Out = ss(l)
    }
}
