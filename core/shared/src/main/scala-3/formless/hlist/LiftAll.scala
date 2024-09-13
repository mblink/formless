package formless.hlist

/**
 * Typeclass witnessing that all the elements of a `HList` have instances of the given typeclass `F`.
 */
trait LiftAll[F[_], T] extends Serializable {
  type Out
  def instances: Out
}

object LiftAll {
  type Aux[F[_], T, O] = LiftAll[F, T] { type Out = O }

  final class Curried[F[_]](private val dummy: Boolean = false) extends AnyVal {
    final def apply[In](in: In)(using l: LiftAll[F, In]): LiftAll.Aux[F, In, l.Out] = l
  }

  def apply[F[_]]: Curried[F] = new Curried[F]
  def apply[F[_], In](using l: LiftAll[F, In]): LiftAll.Aux[F, In, l.Out] = l

  final class Inst[F[_], T, O](o: O) extends LiftAll[F, T], Serializable {
    final type Out = O
    final val instances = o
  }

  inline given liftAllHList[F[_], T <: HList]: LiftAll.Aux[F, T, HList.Map[T, F]] = Inst(summonAllHList[HList.Map[T, F]])
}
