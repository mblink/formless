package formless
package hlist

import scala.quoted.*

/**
 * Type class supporting removal of an element from this `HList`. Available only if this `HList` contains an
 * element of type `E`.
 */
@scala.annotation.implicitNotFound("Cannot remove an element of type ${E} from ${L}: no element has that type, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait Remove[L, E] extends DepFn1[L], Serializable {
  def reinsert(out: Out): L
}

object Remove extends RemoveFromElem {
  type Aux[L, E, O] = Remove[L, E] { type Out = O }

  inline def apply[L, E](using r: Remove[L, E]): Remove.Aux[L, E, r.Out] = r

  private[hlist] final class Indexed[L <: HList, U, A <: HList, B <: HList](index: Int) extends Remove[L, U] {
    type Out = (U, A)
    def apply(l: L): Out = HList.removedAt(l, index).asInstanceOf[Out]
    def reinsert(out: Out): L = HList.insertedAt(out._2, index, out._1).asInstanceOf[L]
  }

  private def derive[L <: HList: Type, U: Type](using Quotes): Expr[Remove[L, U]] =
    ElemOp.materialize[L, U, ElemOp.Remove, Remove[L, U]] { q =>
      import q.reflect.*
      (TypeRepr.of[Indexed[HNil, Any, HNil, HNil]], List(TypeRepr.of[L], TypeRepr.of[U]))
    }

  transparent inline given derived[L <: HList, U]: Remove[L, U] = ${ derive[L, U] }
}

private[hlist] trait RemoveFromElem {
  given removeAt[L <: HList, E](using f: ElemOp[L, E, ElemOp.Remove]): Remove.Aux[L, E, (E, f.A)] =
    new Remove.Indexed[L, E, f.A, f.B](f.index)
}
