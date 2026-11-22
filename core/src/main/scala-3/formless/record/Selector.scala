package formless.record

import scala.quoted.*
import formless.hlist.{DepFn1, HList}

/**
 * Type class supporting record field selection.
 */
@scala.annotation.implicitNotFound("Cannot select field ${Key} from record ${T}: no field has that key, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait Selector[T, Key] extends DepFn1[T], Serializable

object Selector extends SelectorFromField {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[T, K](using s: Selector[T, K]): Selector.Aux[T, K, s.Out] = s

  private[formless] final class Indexed[T <: HList, K, O](index: Int) extends Selector[T, K] {
    type Out = O
    def apply(t: T): Out = t.unsafeApply(index).asInstanceOf[Out]
  }

  private def derive[T <: HList: Type, K: Type](using Quotes): Expr[Selector[T, K]] =
    FieldOp.materialize[T, K ->> Any, FieldOp.Select, Selector[T, K]](
      [O] => (out: Type[O], index: Int) => {
        given Type[O] = out
        '{ new Indexed[T, K, O](${ Expr(index) }) }
      },
    )

  transparent inline given derived[T <: HList, K]: Selector[T, K] = ${ derive[T, K] }
}

// A dependent result lets the compiler infer an expected Out and preserves the named given.
private[formless] trait SelectorFromField {
  given selectorAt[T <: HList, K](using f: FieldOp[T, K ->> Any, FieldOp.Select]): Selector.Aux[T, K, f.Out] =
    new Selector.Indexed[T, K, f.Out](f.index)
}
