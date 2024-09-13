package formless
package record

import formless.hlist.*
import scala.quoted.*

private[formless] sealed trait FindField[T <: HList, F, Cmp[_, _]] {
  type Key
  type Value
  type Replaced[A] <: HList
  type Removed <: HList
  val index: Int
}

object FindField {
  type Aux[T <: HList, F, Cmp[_, _], K, V, Rep[_] <: HList, Rem <: HList] = FindField[T, F, Cmp] {
    type Key = K
    type Value = V
    type Replaced[A] = Rep[A]
    type Removed = Rem
  }

  inline def apply[T <: HList, F, Cmp[_, _]](
    using f: FindField[T, F, Cmp],
  ): FindField.Aux[T, F, Cmp, f.Key, f.Value, f.Replaced, f.Removed] = f

  final class Inst[T <: HList, F, Cmp[_, _], K, V, Rep[_] <: HList, Rem <: HList](val index: Int) extends FindField[T, F, Cmp] {
    final type Key = K
    final type Value = V
    final type Replaced[A] = Rep[A]
    final type Removed = Rem
  }

  private def findFieldInstImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using q: Quotes): Expr[FindField[T, F, Cmp]] = {
    import q.reflect.*

    def go[
      Rep[_] <: HList: Type,
      Rem <: HList: Type,
      RestT <: HList: Type,
    ](idx: Int): Expr[FindField[T, F, Cmp]] =
      Type.of[RestT] match {
        case '[::[head, tail]] if Expr.summon[Cmp[head, F]].nonEmpty =>
          TypeRepr.of[head].typeArgs.map(_.asType) match {
            case List('[key], '[value]) =>
              '{ FindField.Inst[T, F, Cmp, key, value, [a] =>> HList.Concat[Rep[a], tail], HList.Concat[Rem, tail]](${ Expr(idx) }) }
            case ts => report.errorAndAbort(s"Unexpected type args: $ts")
          }
        case '[::[head, tail]] =>
          go[[a] =>> HList.Append[Rep[head], a], HList.Append[Rem, head], tail](idx + 1)
        case '[HNil] =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
      }

    go[[a] =>> a :: HNil, HNil, T](0)
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}
