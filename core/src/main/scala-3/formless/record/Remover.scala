package formless
package record

import formless.hlist.{DepFn1, HList}
import scala.quoted.*

/**
 * Type class supporting record field removal.
 */
@scala.annotation.implicitNotFound("Cannot remove field ${K} from record ${T}: no field has that key, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait Remover[T, K] extends DepFn1[T], Serializable

object Remover extends RemoverFromField {
  type Aux[T, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T, K](using r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T, K](t: T, @annotation.nowarn("msg=unused") k: K)(using r: Remover[T, K]): r.Out = r(t)

  private[formless] final class Indexed[T <: HList, K, O](index: Int) extends Remover[T, K] {
    type Out = O
    def apply(t: T): Out = HList.removedAt(t, index).asInstanceOf[Out]
  }

  private def derive[T <: HList: Type, K: Type](using Quotes): Expr[Remover[T, K]] =
    FieldOp.materialize[T, K ->> Any, FieldOp.Remove, Remover[T, K]](
      [O] => (out: Type[O], index: Int) => {
        given Type[O] = out
        '{ new Indexed[T, K, O](${ Expr(index) }) }
      },
    )

  transparent inline given derived[T <: HList, K]: Remover[T, K] = ${ derive[T, K] }
}

// A dependent result lets the compiler infer an expected Out and preserves the named given.
private[formless] trait RemoverFromField {
  given removerAt[T <: HList, K](using f: FieldOp[T, K ->> Any, FieldOp.Remove]): Remover.Aux[T, K, f.Out] =
    new Remover.Indexed[T, K, f.Out](f.index)
}
