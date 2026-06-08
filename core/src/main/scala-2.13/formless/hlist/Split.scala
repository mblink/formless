package formless
package hlist

/**
 * Type class supporting splitting this `HList` at the `N`th element returning the prefix and suffix as a pair.
 * Available only if this `HList` has at least `N` elements.
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

  def apply[T, N](implicit s: Split[T, N]): Split.Aux[T, N, s.Prefix, s.Suffix] = s

  implicit def splitHList[L <: HList, I <: Int, N <: shapeless.Nat, P <: HList, S <: HList](
    implicit @annotation.unused in: IntToNat.Aux[I, N],
    ss: shapeless.ops.hlist.Split.Aux[L, N, P, S],
  ): Split.Aux[L, I, P, S] =
    new Split[L, I] {
      type Prefix = P
      type Suffix = S
      def apply(l: L): Out = ss(l)
    }
}
