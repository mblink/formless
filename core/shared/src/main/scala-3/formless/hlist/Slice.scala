package formless.hlist

import compiletime.ops.int.{-, >=}

/**
 * Type class supporting the slicing of a `HList`.
 */
trait Slice[N, U, L] extends DepFn1[L] with Serializable

object Slice {
  type Aux[N, U, L, O] = Slice[N, U, L] { type Out = O }

  inline def apply[N, U, L](using s: Slice[N, U, L]): Slice.Aux[N, U, L, s.Out] = s

  given sliceHList[N <: Int, U <: Int, L <: HList](
    using sizeEv: (HList.Size[L] >= U) =:= true,
    rangeEv: (U >= N) =:= true,
    nv: ValueOf[N],
    uv: ValueOf[U]
  ): Slice.Aux[N, U, L, HList.Take[HList.Drop[L, N], U - N]] =
    new Slice[N, U, L] {
      type Out = HList.Take[HList.Drop[L, N], U - N]
      private lazy val n = nv.value
      private lazy val u = uv.value
      def apply(l: L): Out = {
        val a = l.toArray
        HList.fromArray(a.drop(n).take(u - n)).asInstanceOf[Out]
      }
    }
}
