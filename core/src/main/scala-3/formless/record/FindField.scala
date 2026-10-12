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

    val labelledSym = TypeRepr.of[->>].typeSymbol
    val consSym = TypeRepr.of[Any :: HNil].typeSymbol
    val consTpe = consSym.typeRef
    val hnilTpe = TypeRepr.of[HNil]
    val instSym = TypeRepr.of[Inst[HNil, Any, =:=, Any, Any, [a] =>> HNil, HNil]].typeSymbol
    val fTpe = TypeRepr.of[F]
    val cmpTpe = TypeRepr.of[Cmp]

    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consTpe.appliedTo(List(h, t))

    def labelledArgs(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t.dealias match {
        case AppliedType(tycon, List(k, v)) if tycon.typeSymbol == labelledSym => Some((k, v))
        case _ => None
      }

    // `->>` is invariant in its key, so a field whose key is a different constant can never satisfy `Cmp[field, F]`
    val fKey: Option[Constant] = labelledArgs(fTpe).flatMap {
      case (ConstantType(c), _) => Some(c)
      case _ => None
    }

    def keyCannotMatch(head: TypeRepr): Boolean =
      fKey.exists(fk =>
        labelledArgs(head).exists {
          case (ConstantType(c), _) => c.value != fk.value
          case _ => false
        }
      )

    def cmpHolds(head: TypeRepr): Boolean =
      !keyCannotMatch(head) && (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
        case _: ImplicitSearchSuccess => true
        case _ => false
      })

    def found(revInit: List[TypeRepr], head: TypeRepr, tail: TypeRepr, idx: Int): Expr[FindField[T, F, Cmp]] =
      labelledArgs(head) match {
        case Some((key, value)) =>
          val replaced = TypeLambda(
            List("a"),
            _ => List(TypeBounds.empty),
            tl => revInit.foldLeft(cons(tl.param(0), tail))((acc, h) => cons(h, acc)),
          )
          val removed = revInit.foldLeft(tail)((acc, h) => cons(h, acc))
          val targs = List(TypeRepr.of[T], fTpe, cmpTpe, key, value, replaced, removed)
          New(Inferred(instSym.typeRef.appliedTo(targs)))
            .select(instSym.primaryConstructor)
            .appliedToTypes(targs)
            .appliedTo(Literal(IntConstant(idx)))
            .asExprOf[FindField[T, F, Cmp]]
        case None =>
          report.errorAndAbort(s"Unexpected type: ${head.show}")
      }

    @annotation.tailrec
    def go(rest: TypeRepr, revInit: List[TypeRepr], idx: Int): Expr[FindField[T, F, Cmp]] =
      rest.dealias.simplified match {
        case t if t <:< hnilTpe =>
          report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
        case t =>
          t.baseType(consSym) match {
            case AppliedType(_, List(head, tail)) =>
              if (cmpHolds(head)) found(revInit, head, tail, idx)
              else go(tail, head :: revInit, idx + 1)
            case _ =>
              report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
          }
      }

    go(TypeRepr.of[T], Nil, 0)
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}
