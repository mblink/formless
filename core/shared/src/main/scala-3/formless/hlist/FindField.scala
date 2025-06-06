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

  final class Inst[T <: HList, F, Field0, Hd <: HList, Tl <: HList, Rep[_] <: HList, Rem <: HList, Idx <: Int](val index: Idx)
  extends FindField[T, F] {
    final type Field = Field0
    final type Head = Hd
    final type Tail = Tl
    final type Replaced[A] = Rep[A]
    final type Removed = Rem
    final type Index = Idx
  }

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
            FindField.Inst[
              T,
              F,
              head,
              HList.Reverse[RevHead],
              tail,
              [a] =>> HList.Concat[Rep[a], tail],
              HList.Concat[Rem, tail],
              Idx,
            ](${ Expr(idx) })
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
