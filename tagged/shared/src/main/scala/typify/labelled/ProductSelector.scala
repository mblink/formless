package typify
package labelled

import typify.tuple.Tuple

sealed trait ProductSelector[A, Key] {
  type Out
  def apply(a: A): Out
}

object ProductSelector {
  type Aux[A, K, O] = ProductSelector[A, K] { type Out = O }

  implicit def fromSelector[A <: Tuple, K](implicit s: Selector[A, K]): Aux[A, K, s.Out] =
    new ProductSelector[A, K] {
      type Out = s.Out
      def apply(a: A): Out = s(a)
    }

  implicit def fromLabelledGen[A, R <: Tuple, K](implicit lg: StringLabelledGeneric.Aux[A, R], s: Selector[R, K]): Aux[A, K, s.Out] =
    new ProductSelector[A, K] {
      type Out = s.Out
      def apply(a: A): Out = s(lg.to(a))
    }
}
