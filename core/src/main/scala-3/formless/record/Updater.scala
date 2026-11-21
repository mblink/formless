package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
@scala.annotation.implicitNotFound("Cannot update record ${T} with field ${F}: a field with its key could be in an abstract part of the record (a type parameter or abstract type member), so it can be neither replaced nor safely appended, or it is at different indexes in the arms of a union")
trait Updater[T, F] extends DepFn2[T, F], Serializable

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  /** Replaces the first field `=:=` to `F`, or appends `F` when no field could be, in one walk of the record */
  given updaterAt[T <: HList, F](using u: FieldUpdate[T, F]): Updater.Aux[T, F, u.Out] =
    new Updater[T, F] {
      type Out = u.Out
      def apply(t: T, f: F): Out = u(t, f)
    }
}
