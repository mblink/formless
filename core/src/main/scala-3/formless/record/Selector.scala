package formless.record

import scala.language.implicitConversions
import formless.hlist.{DepFn1, HList}

/**
 * Type class supporting record field selection.
 */
trait Selector[T, Key] extends DepFn1[T], Serializable

object Selector {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[T, K](using s: Selector[T, K]): Selector.Aux[T, K, s.Out] = s

  given selectorAt[T <: HList, K](using f: FieldOp[T, K ->> Any, FieldOp.Select]): Selector.Aux[T, K, f.Out] =
    new Selector[T, K] {
      type Out = f.Out
      def apply(t: T): Out = t.unsafeApply(f.index).asInstanceOf[Out]
    }

  @deprecated("Retained for binary compatibility", "0.8.1")
  def selectorInst[T <: HList, K](using f: FindField[T, K ->> Any, <:<]): Selector.Aux[T, K, f.Value] =
    new Selector[T, K] {
      type Out = f.Value
      def apply(t: T): Out = t.unsafeApply(f.index).asInstanceOf[Out]
    }
}
