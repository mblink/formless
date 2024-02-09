package formless.record

import formless.tuple.DepFn2

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F] with Serializable

sealed trait UpdaterLP {
  final given updaterNotFound[T <: Tuple, F]: Updater.Aux[T, F, Tuple.Append[T, F]] =
    new Updater[T, F] {
      type Out = Tuple.Append[T, F]
      def apply(t: T, f: F): Out = t :* f
    }
}

object Updater extends UpdaterLP {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  given updaterFound[T <: Tuple, F](using ff: FindField[T, F]): Updater.Aux[T, F, ff.Replaced[F]] =
    new Updater[T, F] {
      type Out = ff.Replaced[F]
      def apply(t: T, f: F): Out = {
        val a = t.toArray
        a.update(ff.index, f.asInstanceOf[Object])
        Tuple.fromArray(a).asInstanceOf[Out]
      }
    }
}
