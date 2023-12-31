package formless.tuple

/**
 * Type class supporting reifying a `Tuple` of singleton types.
 */
trait Reify[T] extends DepFn0 with Serializable { type Out }

object Reify {
  inline def apply[T](using r: Reify[T]): Reify.Aux[T, r.Out] = r

  type Aux[T, O] = Reify[T] { type Out = O }

  inline given reifyInst[T <: Tuple]: Reify.Aux[T, T] =
    new Reify[T] {
      type Out = T
      def apply(): T =
        compiletime.summonAll[Tuple.Map[T, ValueOf]]
          .map[[a] =>> Any]([t] => (t: t) => t.asInstanceOf[ValueOf[_]].value)
          .asInstanceOf[T]
    }
}
