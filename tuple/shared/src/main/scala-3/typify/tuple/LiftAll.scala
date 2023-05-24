package typify.tuple

trait LiftAll[F[_], T] {
  type Out
  def instances: Out
}

object LiftAll {
  type Aux[F[_], T, O] = LiftAll[F, T] { type Out = O }

  final class Curried[F[_]](private val dummy: Boolean = false) extends AnyVal {
    final def apply[In](in: In)(implicit l: LiftAll[F, In]): Aux[F, In, l.Out] = l
  }

  def apply[F[_]]: Curried[F] = new Curried[F]
  def apply[F[_], In](implicit l: LiftAll[F, In]): Aux[F, In, l.Out] = l

  implicit def emptyTuple[F[_]]: LiftAll.Aux[F, EmptyTuple, EmptyTuple] = new LiftAll[F, EmptyTuple] {
    type Out = EmptyTuple
    def instances = EmptyTuple
  }

  implicit def hcons[F[_], H, T <: Tuple, TI <: Tuple](implicit h: F[H], t: Aux[F, T, TI]): Aux[F, H *: T, F[H] *: TI] =
    new LiftAll[F, H *: T] {
      type Out = F[H] *: TI
      def instances = h *: t.instances
    }
}
