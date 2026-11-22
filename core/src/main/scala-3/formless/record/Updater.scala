package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
@scala.annotation.implicitNotFound("Cannot update record ${T} with field ${F}: the lookup may encounter a field with the same key and a strictly narrower type, a matching field could be in an abstract part of the record, or the match could differ between the arms of a union")
trait Updater[T, F] extends DepFn2[T, F], Serializable

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  /**
   * Replaces the first field `=:=` to `F`, or appends `F` when no field could match. Refuses to skip a field with
   * the same key and a strictly narrower type.
   */
  given updaterAt[T <: HList, F](using u: FieldUpdate[T, F]): Updater.Aux[T, F, u.Out] =
    u
}
