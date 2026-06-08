package formless.hlist

import compiletime.ops.int.{+, >=}

/**
 * Type class supporting the patching of a `HList`.
 */
trait Patcher[N, M, L, In] extends DepFn2[L, In], Serializable

object Patcher {
  type Aux[N, M, L, In, O] = Patcher[N, M, L, In] { type Out = O }

  inline def apply[N, M, L, In](using p: Patcher[N, M, L, In]): Patcher.Aux[N, M, L, In, p.Out] = p

  given patchHList[N <: Int, M <: Int, L <: HList, In <: HList](
    using ev: (HList.Size[L] >= N) =:= true,
    nv: ValueOf[N],
    mv: ValueOf[M],
  ): Patcher.Aux[N, M, L, In, HList.Concat[HList.Take[L, N], HList.Concat[In, HList.Drop[L, N + M]]]] =
    new Patcher[N, M, L, In] {
      type Out = HList.Concat[HList.Take[L, N], HList.Concat[In, HList.Drop[L, N + M]]]
      private lazy val n = nv.value
      private lazy val m = mv.value
      def apply(l: L, in: In): Out = {
        val a = l.toArray
        HList.fromArray(a.take(n) ++ (in.toArray ++ a.drop(n + m))).asInstanceOf[Out]
      }
    }
}
