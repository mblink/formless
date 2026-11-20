package formless.hlist

import compiletime.ops.int.>=

/**
 * Type class supporting splitting this `HList` at the first occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
trait ReverseSplitLeft[L, U] extends DepFn1[L], Serializable {
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

  given reverseSplitLeftAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.ReverseSplitLeft]): ReverseSplitLeft.Aux[L, U, f.A, f.B] =
    new ReverseSplitLeft[L, U] {
      type Prefix = f.A
      type Suffix = f.B
      def apply(l: L): Out = {
        val prefix = HList.take0(l, f.index).toArray.foldLeft(HNil: HList)((acc, x) => x :: acc)
        (prefix, HList.drop0(l, f.index)).asInstanceOf[Out]
      }
    }

  @deprecated("Retained for binary compatibility", "0.8.1")
  private[hlist] def reverseSplitLeftHList[L <: HList, U](
    using f: FindField[L, U],
    ev: (HList.Size[L] >= f.Index) =:= true,
  ): ReverseSplitLeft.Aux[L, U, HList.Reverse[HList.Take[L, f.Index]], HList.Drop[L, f.Index]] =
    new ReverseSplitLeft[L, U] {
      type Prefix = HList.Reverse[HList.Take[L, f.Index]]
      type Suffix = HList.Drop[L, f.Index]
      def apply(l: L): Out = {
        (HList.reverse(HList.take(l, f.index)), HList.drop(l, f.index))
      }

        // (HList.fromArray(HList.take(l, f.index).toArray.reverse).asInstanceOf[Prefix], l.drop(f.index).asInstanceOf[Suffix])
    }
}
