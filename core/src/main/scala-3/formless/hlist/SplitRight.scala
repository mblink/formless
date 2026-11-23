package formless.hlist

import scala.quoted.*

/**
 * Type class supporting splitting this `HList` at the last occurrence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 */
@scala.annotation.implicitNotFound("Cannot split ${L} at its last element of type ${U}: no element has that type, a later one could be in an abstract part of the type (a type parameter or abstract type member), or it is at different indexes in the arms of a union")
trait SplitRight[L, U] extends DepFn1[L], Serializable {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def apply(l: L): Out
}

object SplitRight extends SplitRightFromElem {
  type Aux[L, U, P, S] = SplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: SplitRight[T, U]): SplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  private[hlist] final class Indexed[L <: HList, U, A <: HList, B <: HList](index: Int) extends SplitRight[L, U] {
    type Prefix = A
    type Suffix = B
    def apply(l: L): Out = HList.splitAfter(l, index).asInstanceOf[Out]
  }

  private def derive[L <: HList: Type, U: Type](using Quotes): Expr[SplitRight[L, U]] =
    ElemOp.materialize[L, U, ElemOp.SplitRight, SplitRight[L, U]] { q =>
      import q.reflect.*
      (TypeRepr.of[Indexed[HNil, Any, HNil, HNil]], List(TypeRepr.of[L], TypeRepr.of[U]))
    }

  transparent inline given derived[L <: HList, U]: SplitRight[L, U] = ${ derive[L, U] }
}

private[hlist] trait SplitRightFromElem {
  given splitRightAt[L <: HList, U](using f: ElemOp[L, U, ElemOp.SplitRight]): SplitRight.Aux[L, U, f.A, f.B] =
    new SplitRight.Indexed[L, U, f.A, f.B](f.index)
}
