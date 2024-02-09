package formless
package tuple

import scala.quoted.*

private[formless] sealed trait FindField[T <: Tuple, F] {
  type Field
  type Head <: Tuple
  type Tail <: Tuple
  type Replaced[A] <: Tuple
  type Removed <: Tuple
  val index: Int
}

object FindField {
  type Aux[T <: Tuple, F, Field0, Hd <: Tuple, Tl <: Tuple, Rep[_] <: Tuple, Rem <: Tuple] = FindField[T, F] {
    type Field = Field0
    type Head = Hd
    type Tail = Tl
    type Replaced[A] = Rep[A]
    type Removed = Rem
  }

  inline def apply[T <: Tuple, F](using f: FindField[T, F]): FindField.Aux[T, F, f.Field, f.Head, f.Tail, f.Replaced, f.Removed] = f

  private def foundFieldInstImpl[T <: Tuple: Type, F: Type](using q: Quotes): Expr[FindField[T, F]] = {
    import q.reflect.*

    def go[
      RevHead <: Tuple: Type,
      Rep[_] <: Tuple: Type,
      Rem <: Tuple: Type,
      RestT <: Tuple: Type,
    ](idx: Int): Expr[FindField[T, F]] =
      Type.of[RestT] match {
        case '[*:[head, tail]] if Expr.summon[head =:= F].nonEmpty =>
          '{
            (new FindField[T, F] {
              type Field = head
              type Head = ReverseT[RevHead]
              type Tail = tail
              type Replaced[A] = Tuple.Concat[Rep[A], tail]
              type Removed = Tuple.Concat[Rem, tail]
              val index = ${ Expr(idx) }
            }).asInstanceOf[FindField.Aux[
              T,
              F,
              head,
              ReverseT[RevHead],
              tail,
              [a] =>> Tuple.Concat[Rep[a], tail],
              Tuple.Concat[Rem, tail],
            ]]
          }
        case '[*:[head, tail]] =>
          go[head *: RevHead, [a] =>> Tuple.Append[Rep[head], a], Tuple.Append[Rem, head], tail](idx + 1)
        case '[EmptyTuple] =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in tuple ${Type.show[T]}")
      }

    go[EmptyTuple, [a] =>> a *: EmptyTuple, EmptyTuple, T](0)
  }

  transparent inline given foundFieldInst[T <: Tuple, F]: FindField[T, F] = ${ foundFieldInstImpl[T, F] }
}
