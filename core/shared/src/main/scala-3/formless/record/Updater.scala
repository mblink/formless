package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F] with Serializable

sealed trait UpdaterLP {
  final given updaterNotFound[T <: HList, F]: Updater.Aux[T, F, HList.Append[T, F]] =
    new Updater[T, F] {
      type Out = HList.Append[T, F]
      def apply(t: T, f: F): Out = HList.append(t, f)
    }
}

object Updater extends UpdaterLP {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  given updaterFound[T <: HList, F](using ff: FindField[T, F, =:=]): Updater.Aux[T, F, ff.Replaced[F]] =
    new Updater[T, F] {
      type Out = ff.Replaced[F]
      def apply(t: T, f: F): Out = {
        val a = t.toArray
        a.update(ff.index, f)
        HList.fromArray(a).asInstanceOf[Out]
      }
    }
}
