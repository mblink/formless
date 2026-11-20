package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F], Serializable

sealed trait UpdaterLP {
  @deprecated("Retained for binary compatibility", "0.8.1")
  private[record] final def updaterNotFound[T <: HList, F]: Updater.Aux[T, F, HList.Append[T, F]] = appendField[T, F]

  private def appendField[T <: HList, F]: Updater.Aux[T, F, HList.Append[T, F]] =
    new Updater[T, F] {
      type Out = HList.Append[T, F]
      def apply(t: T, f: F): Out = HList.append(t, f)
    }
}

object Updater extends UpdaterLP {
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

  @deprecated("Retained for binary compatibility", "0.8.1")
  private[record] def updaterFound[T <: HList, F](using ff: FindField[T, F, =:=]): Updater.Aux[T, F, ff.Replaced[F]] =
    new Updater[T, F] {
      type Out = ff.Replaced[F]
      def apply(t: T, f: F): Out = HList.updatedAt(t, ff.index, _ => f).asInstanceOf[Out]
    }
}
