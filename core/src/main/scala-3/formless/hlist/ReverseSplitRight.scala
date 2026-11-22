package formless.hlist

import scala.quoted.*

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at its last element of type ${U}: no element has that type, a later one could be in an abstract part of the type (a type parameter or abstract type member), or it is at different indexes in the arms of a union")
trait ReverseSplitRight[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object ReverseSplitRight extends ReverseSplitRightFromElem {
  type Aux[L, U, P, S] = ReverseSplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: ReverseSplitRight[T, U]): ReverseSplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  private[hlist] final class Indexed[L <: HList, U, A <: HList, B <: HList](index: Int) extends ReverseSplitRight[L, U] {
    type Prefix = A
    type Suffix = B
    def apply(l: L): Out = HList.reverseSplitAfter(l, index).asInstanceOf[Out]
  }

  private def derive[L <: HList: Type, U: Type](using Quotes): Expr[ReverseSplitRight[L, U]] =
    ElemOp.materialize[L, U, ElemOp.ReverseSplitRight, ReverseSplitRight[L, U]] { q =>
      import q.reflect.*
      (TypeRepr.of[Indexed[HNil, Any, HNil, HNil]], List(TypeRepr.of[L], TypeRepr.of[U]))
    }

  transparent inline given derived[L <: HList, U]: ReverseSplitRight[L, U] = ${ derive[L, U] }
}

private[hlist] trait ReverseSplitRightFromElem {
  given reverseSplitRightAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.ReverseSplitRight]): ReverseSplitRight.Aux[L, U, f.A, f.B] =
    new ReverseSplitRight.Indexed[L, U, f.A, f.B](f.index)
}
