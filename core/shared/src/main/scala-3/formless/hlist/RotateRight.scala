package formless.hlist

import compiletime.ops.int.{-, %}

/**
 * Type class supporting rotating a `HList` right.
 */
trait RotateRight[L, N] extends DepFn1[L] with Serializable

sealed trait RotateRightLP {
  final type Aux[L, N, O] = RotateRight[L, N] { type Out = O }

  final given rotateRightHList[L <: HList, N <: Int](using nv: ValueOf[N], sizev: ValueOf[HList.Size[L]])
    : RotateRight.Aux[L, N, HList.Concat[HList.Drop[L, HList.Size[L] - (N % HList.Size[L])], HList.Take[L, HList.Size[L] - (N % HList.Size[L])]]] =
    new RotateRight[L, N] {
      type Out = HList.Concat[HList.Drop[L, HList.Size[L] - (N % HList.Size[L])], HList.Take[L, HList.Size[L] - (N % HList.Size[L])]]
      private lazy val n = nv.value
      private lazy val size = sizev.value
      private lazy val idx = size - (n % size)
      def apply(l: L): Out = {
        val a = l.toArray
        HList.fromArray(a.drop(idx) ++ a.take(idx)).asInstanceOf[Out]
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
