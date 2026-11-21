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
@scala.annotation.implicitNotFound("Cannot replace an element of type ${U} in ${L}: no element has that type, or it could only be in an abstract part of the type (a type parameter or abstract type member), so it can't be found at compile time")
trait Replacer[L, U, V] extends DepFn2[L, V], Serializable

object Replacer {
  type Aux[L, U, V, O] = Replacer[L, U, V] { type Out = O }

  inline def apply[L, U, V](using r: Replacer[L, U, V]): Replacer.Aux[L, U, V, r.Out] = r

  given replacerAt[T <: HList, U, V](using op: formless.record.ReplaceAt[T, U, formless.record.ReplaceAt.Elem]): Replacer.Aux[T, U, V, (U, op.Replaced[V])] =
    new Replacer[T, U, V] {
      type Out = (U, op.Replaced[V])
      def apply(t: T, v: V): Out = {
        var u: Any = null
        val replaced = HList.updatedAt(t, op.index, x => { u = x; v })
        (u, replaced).asInstanceOf[Out]
      }
    }
}
