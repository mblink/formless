package formless.record

import scala.language.implicitConversions
import scala.quoted.*
import formless.tuple.DepFn1

/**
 * Type class supporting record field selection.
 */
trait Selector[T, Key] extends DepFn1[T] with Serializable

object Selector {
  type Aux[T, K, O] = Selector[T, K] { type Out = O }

  inline def apply[T, K](using s: Selector[T, K]): Selector.Aux[T, K, s.Out] = s

  private def selectorInstImpl[T <: Tuple: Type, K: Type](using Quotes): Expr[Selector[T, K]] =
    new SelectorMacros().inst[T, K]

  transparent inline given selectorInst[T <: Tuple, K]: Selector[T, K] = ${ selectorInstImpl[T, K] }
}

private[formless] final class SelectorMacros()(using override val ctx: Quotes) extends MacroUtils {
  import ctx.reflect.*

  final def inst[T <: Tuple: Type, K: Type]: Expr[formless.record.Selector[T, K]] =
    withField[T, K ->> Any, Expr[formless.record.Selector[T, K]]](
      [ReplaceField[_] <: Tuple, RemoveField <: Tuple, k, V] =>
        (_: Type[ReplaceField], _: Type[RemoveField], _: Type[k], _: Type[V]) ?=> (idx: Int) => '{
          (new formless.record.Selector[T, K] {
            type Out = V
            def apply(t: T): Out = t.productElement(${ Expr(idx) }).asInstanceOf[V]
          }).asInstanceOf[formless.record.Selector.Aux[T, K, V]]
        },
      () => report.errorAndAbort(s"Failed to find field ${Type.show[K]} in record ${Type.show[T]}")
    )
}

sealed trait SelectorFromKey[T <: Tuple, K] extends Selector[T, K]

object SelectorFromKey {
  type Aux[T <: Tuple, K, O] = SelectorFromKey[T, K] { type Out = O }

  implicit def selectorFromKeyInst[T <: Tuple, K <: Singleton](k: K)(using s: Selector[T, K]): SelectorFromKey.Aux[T, K, s.Out] =
    new SelectorFromKey[T, K] {
      type Out = s.Out
      def apply(t: T): Out = s(t)
    }
}
