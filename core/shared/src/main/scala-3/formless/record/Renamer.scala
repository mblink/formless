package formless.record

import formless.tuple.DepFn1

/**
 * Type class supporting renaming of a record field.
 */
trait Renamer[T <: Tuple, K1, K2] extends DepFn1[T] with Serializable {
  type Out
  def apply(t: T): Out
}

sealed trait RenamerLP {
  final given tailRenamer[H, T <: Tuple, K1, K2, V](using t: Renamer[T, K1, K2] { type Out <: Tuple }): Renamer.Aux[H *: T, K1, K2, H *: t.Out] =
    new Renamer[H *: T, K1, K2] {
      type Out = H *: t.Out
      def apply(l: H *: T): Out = l.head *: t(l.tail)
    }
}

object Renamer extends RenamerLP {
  type Aux[T <: Tuple, K1, K2, Out0] = Renamer[T, K1, K2] { type Out = Out0 }

  inline def apply[T <: Tuple, K1, K2](using r: Renamer[T, K1, K2]): Renamer.Aux[T, K1, K2, r.Out] = r

  given headRenamer[T <: Tuple, K1, K2, V]: Renamer.Aux[(K1 ->> V) *: T, K1, K2, (K2 ->> V) *: T] =
    new Renamer[(K1 ->> V) *: T, K1, K2] {
      type Out = (K2 ->> V) *: T
      def apply(l: (K1 ->> V) *: T): Out = label[K2](l.head : V) *: l.tail
    }
}
