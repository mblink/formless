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
              '{
                (new FindField[T, F, Cmp] {
                  type Key = key
                  type Value = value
                  type Replaced[A] = HList.Concat[Rep[A], tail]
                  type Removed = HList.Concat[Rem, tail]
                  val index = ${ Expr(idx) }
                }).asInstanceOf[FindField.Aux[T, F, Cmp, key, value, [a] =>> HList.Concat[Rep[a], tail], HList.Concat[Rem, tail]]]
              }
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
