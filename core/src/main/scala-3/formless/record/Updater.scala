package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F], Serializable

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  /** Replaces the first field `=:=` to `F`, or appends `F` when no field could be, in one walk of the record */
  given updaterAt[T <: HList, F](using op: FieldOp[T, F, FieldOp.Update]): Updater.Aux[T, F, op.Out] =
    new Updater[T, F] {
      type Out = op.Out
      def apply(t: T, f: F): Out =
        (if (op.index < 0) HList.append(t, f) else HList.updatedAt(t, op.index, _ => f)).asInstanceOf[Out]
    }
}
