package formless.hlist

import compiletime.ops.int.>

/**
 * Type class supporting replacement of the `N`th element of this `HList` with an element of type `V`.
 * Available only if this `HList` contains at least `N` elements.
 */
trait ReplaceAt[L, N, V] extends DepFn2[L, V], Serializable

object ReplaceAt {
  type Aux[L, N, V, O] = ReplaceAt[L, N, V] { type Out = O }

  inline def apply[L, N, V](using r: ReplaceAt[L, N, V]): ReplaceAt.Aux[L, N, V, r.Out] = r

  given replaceAtHList[L <: HList, N <: Int, V](
    using ev: (HList.Size[L] > N) =:= true,
    nv: ValueOf[N],
  ): ReplaceAt.Aux[L, N, V, (HList.Elem[L, N], ReplaceAtIndex[L, N, V])] =
    new ReplaceAt[L, N, V] {
      type Out = (HList.Elem[L, N], ReplaceAtIndex[L, N, V])
      private lazy val n = nv.value
      def apply(l: L, v: V): Out = {
        val a = l.toArray
        (a(n), HList.fromArray(a.patch(n, List(v), 1))).asInstanceOf[Out]
      }
    }
}
