package formless
package record

import formless.tuple.DepFn1
import scala.quoted.*

/**
 * Type class supporting record field removal.
 */
trait Remover[T, K] extends DepFn1[T] with Serializable

object Remover {
  type Aux[T, K, O] = Remover[T, K] { type Out = O }

  inline def apply[T, K](using r: Remover[T, K]): Remover.Aux[T, K, r.Out] = r
  inline def apply[T, K](t: T, k: K)(using r: Remover[T, K]): r.Out = r(t)

  private def removerInstImpl[T <: Tuple: Type, K: Type](using Quotes): Expr[Remover[T, K]] =
    new RemoverMacros().instImpl[T, K]

  inline transparent given removerInst[T <: Tuple, K]: Remover[T, K] = ${ removerInstImpl[T, K] }
}

private[formless] final class RemoverMacros()(using override val ctx: Quotes) extends MacroUtils {
  import ctx.reflect.*

  final def instImpl[T <: Tuple: Type, K: Type]: Expr[Remover[T, K]] =
    withField[T, K ->> Any, Expr[Remover[T, K]]](
      [ReplaceField[_] <: Tuple, RemoveField <: Tuple, k, V] =>
        (_: Type[ReplaceField], _: Type[RemoveField], _: Type[k], _: Type[V]) ?=> (idx: Int) => '{
          (new Remover[T, K] {
            type Out = (V, RemoveField)
            def apply(t: T): Out = {
              val a = t.toArray
              (a(${ Expr(idx) }), Tuple.fromArray(a.patch(${ Expr(idx) }, Nil, 1))).asInstanceOf[Out]
            }
          }).asInstanceOf[Remover.Aux[T, K, (V, RemoveField)]]
        },
      () => report.errorAndAbort(s"Failed to find field ${Type.show[K]} in record ${Type.show[T]}")
    )
}
