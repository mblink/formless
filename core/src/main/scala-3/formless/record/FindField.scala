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

  private class ConcreteLabelled[K, V]

  private def findFieldInstImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using q: Quotes): Expr[FindField[T, F, Cmp]] = {
    import q.reflect.*

    val labelled = TypeRepr.of[->>]
    val labelledSym = labelled.typeSymbol
    val concreteLabelled = TypeRepr.of[ConcreteLabelled]
    val concreteLabelledSym = concreteLabelled.typeSymbol
    val consTpe = TypeRepr.of[Any :: HNil].typeSymbol.typeRef
    val instSym = TypeRepr.of[Inst[HNil, Any, =:=, Any, Any, [a] =>> HNil, HNil]].typeSymbol

    def concrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(labelledSym), List(concreteLabelled))
    def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consTpe.appliedTo(List(h, t))

    // `Replaced` and `Removed` are built as concrete HList types, not as nested `HList.Append`/`HList.Concat` match types
    def found(revInit: List[TypeRepr], key: TypeRepr, value: TypeRepr, tail: TypeRepr, idx: Int): Expr[FindField[T, F, Cmp]] = {
      val replaced = TypeLambda(
        List("a"),
        _ => List(TypeBounds.empty),
        tl => revInit.foldLeft(cons(tl.param(0), tail))((acc, h) => cons(h, acc)),
      )
      val removed = revInit.foldLeft(tail)((acc, h) => cons(h, acc))
      val targs = List(TypeRepr.of[T], TypeRepr.of[F], TypeRepr.of[Cmp], key, value, replaced, removed)
      New(Inferred(instSym.typeRef.appliedTo(targs)))
        .select(instSym.primaryConstructor)
        .appliedToTypes(targs)
        .appliedTo(Literal(IntConstant(idx)))
        .asExprOf[FindField[T, F, Cmp]]
    }

    // `revInit` holds the fields before `RestT`, nearest first
    def go[RestT <: HList: Type](revInit: List[TypeRepr], idx: Int): Expr[FindField[T, F, Cmp]] =
      Type.of[RestT] match {
        case '[::[head, tail]] if Expr.summon[Cmp[head, F]].nonEmpty =>
          // We need to replace `->>` with a concrete `class` type so the match works
          // Matches on opaque types like `->>` will *never* match
          val concretTypeRepr = concrete(TypeRepr.of[head].dealias)
          concretTypeRepr.asType match {
            case '[ConcreteLabelled[k, v]] =>
              found(revInit, unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v]), TypeRepr.of[tail], idx)
            case _ =>
              report.errorAndAbort(s"Unexpected type: ${concretTypeRepr.show}")
          }
        // Checked before `head :: tail`, which `Nothing` also matches, with a `Nothing` tail that would never end the walk
        case _ if TypeRepr.of[RestT] <:< TypeRepr.of[HNil] =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
        case '[::[head, tail]] =>
          go[tail](TypeRepr.of[head] :: revInit, idx + 1)
      }: @annotation.nowarn("msg=match may not be exhaustive")

    go[T](Nil, 0)
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}
