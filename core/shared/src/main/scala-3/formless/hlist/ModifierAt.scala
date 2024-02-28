package formless.hlist

/**
 * Type class supporting replacement of the `N`th element of this `HList` with the result of calling `F` on it.
 * Available only if this `HList` contains at least `N` elements.
 */
trait ModifierAt[L, N, U, V] extends DepFn2[L, U => V] with Serializable

object ModifierAt {
  type Aux[L, N, U, V, O] = ModifierAt[L, N, U, V] { type Out = O }

  inline def apply[L, N, U, V](using r: ModifierAt[L, N, U, V]): ModifierAt.Aux[L, N, U, V, r.Out] = r

  given modifierAtHList[L <: HList, N <: Int, U, V](
    using ev: HList.Elem[L, N] <:< U,
    nv: ValueOf[N],
  ): ModifierAt.Aux[L, N, U, V, (U, ReplaceAtIndex[L, N, V])] =
    new ModifierAt[L, N, U, V] {
      type Out = (U, ReplaceAtIndex[L, N, V])
      private lazy val n = nv.value
      def apply(l: L, f: U => V): Out = {
        val a = l.toArray
        val u = a(n).asInstanceOf[U]
        (u, HList.fromArray(a.patch(n, List(f(u)), 1))).asInstanceOf[Out]
      }
    }
}
