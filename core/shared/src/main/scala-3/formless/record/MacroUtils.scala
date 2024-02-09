package formless
package record

import scala.quoted.*

private[formless] trait MacroUtils {
  implicit val ctx: Quotes

  import ctx.reflect.*

  final private def withFieldRec[
    ReplaceField[_] <: Tuple: Type,
    RemoveField <: Tuple: Type,
    T <: Tuple: Type,
    F: Type, O,
  ](idx: Int)(
    onFound: [ReplaceField[_] <: Tuple, RemoveField <: Tuple, K, V] =>
      (Type[ReplaceField], Type[RemoveField], Type[K], Type[V]) ?=> Int => O,
    onNotFound: () => O,
  ): O =
    Type.of[T] match {
      case '[*:[head, tail]] if Expr.summon[head <:< F].nonEmpty =>
        TypeRepr.of[head].typeArgs.map(_.asType) match {
          case List('[key], '[value]) =>
            onFound[[a] =>> Tuple.Concat[ReplaceField[a], tail], Tuple.Concat[RemoveField, tail], key, value](idx)
          case ts => report.errorAndAbort(s"Unexpected type args: $ts")
        }
      case '[*:[head, tail]] =>
        withFieldRec[[a] =>> Tuple.Append[ReplaceField[head], a], Tuple.Append[RemoveField, head], tail, F, O](idx + 1)(onFound, onNotFound)
      case '[EmptyTuple] => onNotFound()
    }

  final def withField[T <: Tuple: Type, F: Type, O](
    onFound: [ReplaceField[_] <: Tuple, RemoveField <: Tuple, K, V] =>
      (Type[ReplaceField], Type[RemoveField], Type[K], Type[V]) ?=> Int => O,
    onNotFound: () => O,
  ): O =
    withFieldRec[[a] =>> a *: EmptyTuple, EmptyTuple, T, F, O](0)(onFound, onNotFound)
}
