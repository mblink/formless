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

  @deprecated("Retained for binary compatibility", "0.7.0")
  private[hlist] final class Inst[F[_], T, O](o: O) extends LiftAll[F, T], Serializable {
    final type Out = O
    final val instances = o
  }

  given hnil[F[_]]: LiftAll.Aux[F, HNil, HNil] =
    new LiftAll[F, HNil] {
      type Out = HNil
      lazy val instances = HNil
    }

  given hcons[F[_], H, T <: HList, TO <: HList](using h: F[H], t: LiftAll.Aux[F, T, TO]): LiftAll.Aux[F, H :: T, F[H] :: TO] =
    new LiftAll[F, H :: T] {
      type Out = F[H] :: TO
      lazy val instances = h :: t.instances
    }
}
