package formless
package hlist

/**
 * Type class supporting removal of an element from this `HList`. Available only if this `HList` contains an
 * element of type `E`.
 */
trait Remove[L, E] extends DepFn1[L], Serializable {
  def reinsert(out: Out): L
}

object Remove {
  type Aux[L, E, O] = Remove[L, E] { type Out = O }

  inline def apply[L, E](using r: Remove[L, E]): Remove.Aux[L, E, r.Out] = r

  given removeAt[L <: HList, E](using f: ElemOp[L, E, ElemOp.Remove]): Remove.Aux[L, E, (E, f.A)] =
    new Remove[L, E] {
      type Out = (E, f.A)
      def apply(l: L): Out = HList.removedAt(l, f.index).asInstanceOf[Out]
      def reinsert(out: Out): L = HList.insertedAt(out._2.asInstanceOf[HList], f.index, out._1).asInstanceOf[L]
    }
}
