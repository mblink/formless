package formless.hlist

import formless.record.->>

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common supertype.
 */
trait Lub[-A, -B, Out] extends Serializable {
  def left(a: A): Out
  def right(b: B): Out
}

sealed trait LubLP {
  final given lub[T]: Lub[T, T, T] =
    new Lub[T, T, T] {
      def left(a: T): T = a
      def right(b: T): T = b
    }
}

object Lub extends LubLP {
  given lubFields[K1, K2, V]: Lub[K1 ->> V, K2 ->> V, (K1 | K2) ->> V] =
    new Lub[K1 ->> V, K2 ->> V, (K1 | K2) ->> V] {
      def left(a: K1 ->> V): (K1 | K2) ->> V = a.asInstanceOf[(K1 | K2) ->> V]
      def right(b: K2 ->> V): (K1 | K2) ->> V = b.asInstanceOf[(K1 | K2) ->> V]
    }
}
