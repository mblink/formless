package formless.record

import scala.language.implicitConversions
import formless.tuple.DepFn1

/**
 * Type class supporting record field selection.
 */
trait Selector[T, Key] extends DepFn1[T] with Serializable

object Selector {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[T, K](using s: Selector[T, K]): Selector.Aux[T, K, s.Out] = s

  inline given selectorInst[T <: Tuple, K](
    using idx: ValueOf[FieldIndex[T, K]],
  ): Selector.Aux[T, K, FieldValue[T, K]] =
    new Selector[T, K] {
      type Out = FieldValue[T, K]
      def apply(t: T): Out = t.productElement(idx.value).asInstanceOf[Out]
    }
}

sealed trait SelectorFromKey[T <: Tuple, K] extends Selector[T, K]

object SelectorFromKey {
  type Aux[T <: Tuple, K, O] = SelectorFromKey[T, K] { type Out = O }

  implicit def selectorFromKeyInst[T <: Tuple, K <: Singleton](k: K)(using s: Selector[T, K]): SelectorFromKey.Aux[T, K, s.Out] =
    new SelectorFromKey[T, K] {
      type Out = s.Out
      def apply(t: T): Out = s(t)
    }
}
