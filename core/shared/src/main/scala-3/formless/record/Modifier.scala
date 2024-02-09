package formless.record

import formless.tuple.DepFn2
import scala.quoted.*

/**
 * Type class supporting modification of a record field by given function.
 */
trait Modifier[T <: Tuple, K, A, B] extends DepFn2[T, A => B] with Serializable

object Modifier {
  type Aux[T <: Tuple, K, A, B, O] = Modifier[T, K, A, B] { type Out = O }

  inline def apply[T <: Tuple, K, A, B](using m: Modifier[T, K, A, B]): Modifier.Aux[T, K, A, B, m.Out] = m

  private def modifierInstImpl[T <: Tuple: Type, K: Type, A: Type, B: Type](using Quotes): Expr[Modifier[T, K, A, B]] =
    new ModifierMacros().instImpl[T, K, A, B]

  transparent inline given modifierInst[T <: Tuple, K, A, B]: Modifier[T, K, A, B] = ${ modifierInstImpl[T, K, A, B] }
}

private[formless] final class ModifierMacros()(using override val ctx: Quotes) extends MacroUtils {
  import ctx.reflect.*

  final def instImpl[T <: Tuple: Type, K: Type, A: Type, B: Type]: Expr[Modifier[T, K, A, B]] =
    withField[T, K ->> A, Expr[Modifier[T, K, A, B]]](
      [ReplaceField[_] <: Tuple, RemoveField <: Tuple, k, v] =>
        (_: Type[ReplaceField], _: Type[RemoveField], _: Type[k], _: Type[v]) ?=> (idx: Int) => '{
          (new Modifier[T, K, A, B] {
            type Out = ReplaceField[K ->> B]
            def apply(t: T, f: A => B): Out = {
              val a = t.toArray
              a.update(${ Expr(idx) }, f(a(${ Expr(idx) }).asInstanceOf[A]).asInstanceOf[Object])
              Tuple.fromArray(a).asInstanceOf[Out]
            }
          }).asInstanceOf[Modifier.Aux[T, K, A, B, ReplaceField[K ->> B]]]
        },
      () => report.errorAndAbort(s"Failed to find field ${Type.show[K]} in record ${Type.show[T]}")
    )
}
