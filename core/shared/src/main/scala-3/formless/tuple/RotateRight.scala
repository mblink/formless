package formless.tuple

import compiletime.ops.int.{-, %}

/**
 * Type class supporting rotating a `Tuple` right.
 */
trait RotateRight[L, N] extends DepFn1[L] with Serializable

sealed trait RotateRightLP {
  final type Aux[L, N, O] = RotateRight[L, N] { type Out = O }

  final given tupleRotateRight[L <: Tuple, N <: Int](using nv: ValueOf[N], sizev: ValueOf[Tuple.Size[L]])
    : RotateRight.Aux[L, N, Tuple.Concat[Tuple.Drop[L, Tuple.Size[L] - (N % Tuple.Size[L])], Tuple.Take[L, Tuple.Size[L] - (N % Tuple.Size[L])]]] =
    new RotateRight[L, N] {
      type Out = Tuple.Concat[Tuple.Drop[L, Tuple.Size[L] - (N % Tuple.Size[L])], Tuple.Take[L, Tuple.Size[L] - (N % Tuple.Size[L])]]
      private lazy val n = nv.value
      private lazy val size = sizev.value
      private lazy val idx = size - (n % size)
      def apply(l: L): Out = {
        val a = l.toArray
        Tuple.fromArray(a.drop(idx) ++ a.take(idx)).asInstanceOf[Out]
      }
    }
}

object RotateRight extends RotateRightLP {
  inline def apply[L, N](using r: RotateRight[L, N]): RotateRight.Aux[L, N, r.Out] = r

  given rotateRightZero[L]: RotateRight.Aux[L, 0, L] =
    new RotateRight[L, 0] {
      type Out = L
      def apply(l: L): Out = l
    }
}
