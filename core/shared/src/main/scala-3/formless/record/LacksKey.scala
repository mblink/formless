package formless.record

import scala.quoted.*

/**
 * Type class to witness that a record of type `T` does not contain a key of type `K`.
 */
trait LacksKey[L <: Tuple, K]

object LacksKey {
  inline def apply[T <: Tuple, K](using l: LacksKey[T, K]): LacksKey[T, K] = l

  private def lacksKeyInstImpl[T <: Tuple: Type, K: Type](using Quotes): Expr[LacksKey[T, K]] =
    new LacksKeyMacros().inst[T, K]

  inline given lacksKeyInst[T <: Tuple, K]: LacksKey[T, K] = ${ lacksKeyInstImpl[T, K] }
}

private[formless] final class LacksKeyMacros()(using override val ctx: Quotes) extends MacroUtils {
  import ctx.reflect.*

  final def inst[T <: Tuple: Type, K: Type]: Expr[LacksKey[T, K]] =
    withField[T, K ->> Any, Expr[LacksKey[T, K]]](
      [ReplaceField[_] <: Tuple, RemoveField <: Tuple, k, V] =>
        (_: Type[ReplaceField], _: Type[RemoveField], _: Type[k], _: Type[V]) ?=> (_: Int) =>
          report.errorAndAbort(s"Key ${Type.show[K]} found in record ${Type.show[T]}"),
      () => '{ new LacksKey[T, K] {} }
    )
}
