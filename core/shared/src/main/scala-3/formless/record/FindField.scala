package formless
package record

import scala.quoted.*

private[formless] sealed trait FindField[T <: Tuple, F, Cmp[_, _]] {
  type Key
  type Value
  type Replaced[A] <: Tuple
  type Removed <: Tuple
  val index: Int
}

object FindField {
  type Aux[T <: Tuple, F, Cmp[_, _], K, V, Rep[_] <: Tuple, Rem <: Tuple] = FindField[T, F, Cmp] {
    type Key = K
    type Value = V
    type Replaced[A] = Rep[A]
    type Removed = Rem
  }

  inline def apply[T <: Tuple, F, Cmp[_, _]](
    using f: FindField[T, F, Cmp],
  ): FindField.Aux[T, F, Cmp, f.Key, f.Value, f.Replaced, f.Removed] = f

  private def foundFieldInstImpl[T <: Tuple: Type, F: Type, Cmp[_, _]: Type](using q: Quotes): Expr[FindField[T, F, Cmp]] = {
    import q.reflect.*

    def go[
      Rep[_] <: Tuple: Type,
      Rem <: Tuple: Type,
      RestT <: Tuple: Type,
    ](idx: Int): Expr[FindField[T, F, Cmp]] =
      Type.of[RestT] match {
        case '[*:[head, tail]] if Expr.summon[Cmp[head, F]].nonEmpty =>
          TypeRepr.of[head].typeArgs.map(_.asType) match {
            case List('[key], '[value]) =>
              '{
                (new FindField[T, F, Cmp] {
                  type Key = key
                  type Value = value
                  type Replaced[A] = Tuple.Concat[Rep[A], tail]
                  type Removed = Tuple.Concat[Rem, tail]
                  val index = ${ Expr(idx) }
                }).asInstanceOf[FindField.Aux[T, F, Cmp, key, value, [a] =>> Tuple.Concat[Rep[a], tail], Tuple.Concat[Rem, tail]]]
              }
            case ts => report.errorAndAbort(s"Unexpected type args: $ts")
          }
        case '[*:[head, tail]] =>
          go[[a] =>> Tuple.Append[Rep[head], a], Tuple.Append[Rem, head], tail](idx + 1)
        case '[EmptyTuple] =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
      }

    go[[a] =>> a *: EmptyTuple, EmptyTuple, T](0)
  }

  transparent inline given foundFieldInst[T <: Tuple, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ foundFieldInstImpl[T, F, Cmp] }
}
