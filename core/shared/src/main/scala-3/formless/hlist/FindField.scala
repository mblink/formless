package formless
package hlist

import compiletime.ops.int.S
import scala.quoted.*

private[formless] sealed trait FindField[T <: HList, F] {
  type Field
  type Head <: HList
  type Tail <: HList
  type Replaced[A] <: HList
  type Removed <: HList
  type Index <: Int
  val index: Index
}

object FindField {
  type Aux[T <: HList, F, Field0, Hd <: HList, Tl <: HList, Rep[_] <: HList, Rem <: HList, Idx <: Int] = FindField[T, F] {
    type Field = Field0
    type Head = Hd
    type Tail = Tl
    type Replaced[A] = Rep[A]
    type Removed = Rem
    type Index = Idx
  }

  inline def apply[T <: HList, F](using f: FindField[T, F]): FindField.Aux[T, F, f.Field, f.Head, f.Tail, f.Replaced, f.Removed, f.Index] = f

  private def findFieldInstImpl[T <: HList: Type, F: Type](using q: Quotes): Expr[FindField[T, F]] = {
    import q.reflect.*

    def go[
      RevHead <: HList: Type,
      Rep[_] <: HList: Type,
      Rem <: HList: Type,
      RestT <: HList: Type,
      Idx <: Int: Type,
    ](idx: Idx): Expr[FindField[T, F]] =
      Type.of[RestT] match {
        case '[::[head, tail]] if Expr.summon[head =:= F].nonEmpty =>
          '{
            (new FindField[T, F] {
              type Field = head
              type Head = HList.Reverse[RevHead]
              type Tail = tail
              type Replaced[A] = HList.Concat[Rep[A], tail]
              type Removed = HList.Concat[Rem, tail]
              type Index = Idx
              val index = ${ Expr(idx) }
            }).asInstanceOf[FindField.Aux[
              T,
              F,
              head,
              HList.Reverse[RevHead],
              tail,
              [a] =>> HList.Concat[Rep[a], tail],
              HList.Concat[Rem, tail],
              Idx,
            ]]
          }
        case '[::[head, tail]] =>
          go[head :: RevHead, [a] =>> HList.Append[Rep[head], a], HList.Append[Rem, head], tail, S[Idx]]((idx + 1).asInstanceOf[S[Idx]])
        case '[HNil] =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in HList ${Type.show[T]}")
      }

    go[HNil, [a] =>> a :: HNil, HNil, T, 0](0)
  }

  transparent inline given findFieldInst[T <: HList, F]: FindField[T, F] = ${ findFieldInstImpl[T, F] }
}
