package formless.hlist

import compiletime.ops.int.%

/**
 * Type class supporting rotating a `HList` left.
 */
trait RotateLeft[L, N] extends DepFn1[L] with Serializable

sealed trait RotateLeftLP {
  final type Aux[L, N, O] = RotateLeft[L, N] { type Out = O }

  final given rotateLeftHList[L <: HList, N <: Int](
    using nv: ValueOf[N],
    sizev: ValueOf[HList.Size[L]],
  ): RotateLeft.Aux[L, N, HList.Concat[HList.Drop[L, N % HList.Size[L]], HList.Take[L, N % HList.Size[L]]]] =
    new RotateLeft[L, N] {
      type Out = HList.Concat[HList.Drop[L, N % HList.Size[L]], HList.Take[L, N % HList.Size[L]]]
      private lazy val n = nv.value
      private lazy val size = sizev.value
      private lazy val rem = n % size
      def apply(l: L): Out = {
        val a = l.toArray
        HList.fromArray(a.drop(rem) ++ a.take(rem)).asInstanceOf[Out]
      }
    }
}

object RotateLeft extends RotateLeftLP {
  inline def apply[L, N](using r: RotateLeft[L, N]): RotateLeft.Aux[L, N, r.Out] = r

  given rotateLeftZero[L]: RotateLeft.Aux[L, 0, L] =
    new RotateLeft[L, 0] {
      type Out = L
      def apply(l: L): Out = l
    }
}
