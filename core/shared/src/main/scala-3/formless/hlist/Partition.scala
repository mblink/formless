package formless.hlist

import scala.util.NotGiven

/**
 * Type class supporting partitioning this `HList` into those elements of type `U` and the remainder.
 */
trait Partition[L, U] extends DepFn1[L] with Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def filter(l: L): Prefix
  def filterNot(l: L): Suffix

  final def apply(l: L): Out = (filter(l), filterNot(l))
}

object Partition {
  inline def apply[L, U](using p: Partition[L, U]): Partition.Aux[L, U, p.Prefix, p.Suffix] = p

  type Aux[L, U, P, S] = Partition[L, U] {
    type Prefix = P
    type Suffix = S
  }

  given partitionHNil[U]: Partition.Aux[HNil, U, HNil, HNil] =
    new Partition[HNil, U] {
      type Prefix = HNil
      type Suffix = HNil
      def filter(l: HNil): HNil = HNil
      def filterNot(l: HNil): HNil = HNil
    }

  given partitionHCons1[H, L <: HList, LPrefix <: HList, LSuffix <: HList](
    using p: Partition.Aux[L, H, LPrefix, LSuffix],
  ): Partition.Aux[H :: L, H, H :: LPrefix, LSuffix] =
    new Partition[H :: L, H] {
      type Prefix = H :: LPrefix
      type Suffix = LSuffix
      def filter(l: H :: L): Prefix = l.head :: p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = p.filterNot(l.tail)
    }

  given partitionHCons2[H, L <: HList, U, LPrefix <: HList, LSuffix <: HList](
    using ev: NotGiven[U =:= H],
    p: Partition.Aux[L, U, LPrefix, LSuffix],
  ): Partition.Aux[H :: L, U, LPrefix, H :: LSuffix] =
    new Partition[H :: L, U] {
      type Prefix = LPrefix
      type Suffix = H :: LSuffix
      def filter(l: H :: L): Prefix    = p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = l.head :: p.filterNot(l.tail)
    }
}
