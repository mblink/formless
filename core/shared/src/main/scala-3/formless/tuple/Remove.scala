package formless
package tuple

/**
 * Type class supporting removal of an element from this `Tuple`. Available only if this `Tuple` contains an
 * element of type `E`.
 */
trait Remove[L, E] extends DepFn1[L] with Serializable {
  def reinsert(out: Out): L
}

object Remove {
  type Aux[L, E, O] = Remove[L, E] { type Out = O }

  inline def apply[L, E](using r: Remove[L, E]): Remove.Aux[L, E, r.Out] = r

  given removeTuple[L <: Tuple, E](using f: FindField[L, E]): Remove.Aux[L, E, (E, f.Removed)] =
    new Remove[L, E] {
      type Out = (E, f.Removed)
      def apply(l: L): Out = {
        val a = l.toArray
        (a(f.index), Tuple.fromArray(a.patch(f.index, Nil, 1))).asInstanceOf[Out]
      }
      def reinsert(out: Out): L =
        Tuple.fromArray(out._2.toArray.patch(f.index, List(out._1.asInstanceOf[Object]), 0)).asInstanceOf[L]
    }
}
