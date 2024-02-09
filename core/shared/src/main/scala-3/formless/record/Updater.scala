package formless.record

import formless.tuple.DepFn2
import scala.quoted.*

/**
 * Type class supporting record update and extension.
 */
trait Updater[T, F] extends DepFn2[T, F] with Serializable

object Updater {
  type Aux[T, F, O] = Updater[T, F] { type Out = O }

  inline def apply[T, F](using u: Updater[T, F]): Updater.Aux[T, F, u.Out] = u
  inline def apply[T, F](t: T, f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  private def updaterInstImpl[T <: Tuple: Type, F: Type](using Quotes): Expr[Updater[T, F]] =
    new UpdaterMacros().instImpl[T, F]

  inline transparent given updaterInst[T <: Tuple, F]: Updater[T, F] = ${ updaterInstImpl[T, F] }
}

private[formless] final class UpdaterMacros()(using override val ctx: Quotes) extends MacroUtils {
  import ctx.reflect.*

  final def instImpl[T <: Tuple: Type, F: Type]: Expr[Updater[T, F]] =
    withField[T, F, Expr[Updater[T, F]]](
      [ReplaceField[_] <: Tuple, RemoveField <: Tuple, K, V] =>
        (_: Type[ReplaceField], _: Type[RemoveField], _: Type[K], _: Type[V]) ?=> (idx: Int) => '{
          (new Updater[T, F] {
            type Out = ReplaceField[F]
            def apply(t: T, f: F): Out = {
              val a = t.toArray
              a.update(${ Expr(idx) }, f.asInstanceOf[Object])
              Tuple.fromArray(a).asInstanceOf[Out]
            }
          }).asInstanceOf[Updater.Aux[T, F, ReplaceField[F]]]
        },
      () => '{
        (new Updater[T, F] {
          type Out = Tuple.Append[T, F]
          def apply(t: T, f: F): Out = t :* f
        }).asInstanceOf[Updater.Aux[T, F, Tuple.Append[T, F]]]
      }
    )
}
