package formless.record

import scala.language.implicitConversions
import formless.hlist.{DepFn1, HList}

/**
 * Type class supporting record field selection.
 */
@scala.annotation.implicitNotFound("Cannot select field ${Key} from record ${T}: no field has that key, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time, or it is at different indexes in the arms of a union")
trait Selector[T, Key] extends DepFn1[T], Serializable

object Selector {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[T, K](using s: Selector[T, K]): Selector.Aux[T, K, s.Out] = s

  given selectorAt[T <: HList, K](using f: FieldOp[T, K ->> Any, FieldOp.Select]): Selector.Aux[T, K, f.Out] =
    new Selector[T, K] {
      type Out = f.Out
      def apply(t: T): Out = t.unsafeApply(f.index).asInstanceOf[Out]
    }
}
