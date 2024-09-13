package formless
package hlist

type ReplaceAtIndex[L <: HList, I <: Int, A] = ReplaceAtIndex0[L, I, A, 0]

type ReplaceAtIndex0[L <: HList, I <: Int, A, Curr <: Int] <: HList = (L, Curr) match {
  case (_ :: t, I) => A :: t
  case (h :: t, _) => h :: ReplaceAtIndex0[t, I, A, compiletime.ops.int.S[Curr]]
}

/**
 * Type class supporting replacement of the first element of type `U` from this `HList` with an element of type `V`.
 * Available only if this `HList` contains an element of type `U`.
 */
trait Replacer[L, U, V] extends DepFn2[L, V], Serializable

object Replacer {
  type Aux[L, U, V, O] = Replacer[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Replacer[L, U, V]): Replacer.Aux[L, U, V, r.Out] = r

  given replacerHList[T <: HList, U, V](using f: FindField[T, U]): Replacer.Aux[T, U, V, (U, f.Replaced[V])] =
    new Replacer[T, U, V] {
      type Out = (U, f.Replaced[V])
      def apply(t: T, v: V): Out = {
        val a = t.toArray
        val u = a(f.index)
        a.update(f.index, v)
        (u, HList.fromArray(a)).asInstanceOf[Out]
      }
    }
}
