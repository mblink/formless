package formless.hlist

import compiletime.ops.int.>=

/**
 * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
trait SplitLeft[L, U] extends DepFn1[L], Serializable {
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

  given splitLeftHList[L <: HList, U](
    using f: FindField[L, U],
    ev: (HList.Size[L] >= f.Index) =:= true,
  ): SplitLeft.Aux[L, U, HList.Take[L, f.Index], HList.Drop[L, f.Index]] =
    new SplitLeft[L, U] {
      type Prefix = HList.Take[L, f.Index]
      type Suffix = HList.Drop[L, f.Index]
      def apply(l: L): Out = (HList.take(l, f.index), HList.drop(l, f.index))
    }
}
