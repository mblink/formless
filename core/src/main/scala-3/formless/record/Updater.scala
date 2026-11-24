package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
@scala.annotation.implicitNotFound("Cannot update record ${T} with field ${F}: the key may already have a field with a different value type or a strict subtype of the requested field, or the lookup could be undecidable. To change the field's type, use updateWith or remove it first.")
trait Updater[T, F] extends DepFn2[T, F], Serializable

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  /**
   * Replaces the first field of the same key when it is `=:=` to `F`, or appends `F` when the key is absent.
   */
  given updaterAt[T <: HList, F](using u: FieldUpdate[T, F]): Updater.Aux[T, F, u.Out] =
    u
}
