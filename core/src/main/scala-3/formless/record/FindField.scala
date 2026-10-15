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
    val fTpe = TypeRepr.of[F]
    val cmpTpe = TypeRepr.of[Cmp]

    def concrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(labelledSym), List(concreteLabelled))
    def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consTpe.appliedTo(List(h, t))

    // `show` throws CyclicReference for a type owned by a method whose result type is still being inferred
    def display(t: TypeRepr): String = scala.util.Try(t.show).getOrElse(t.typeSymbol.name)

    def labelledArgs(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t.dealias match {
        case AppliedType(tycon, List(k, v)) if tycon.typeSymbol == labelledSym => Some((k, v))
        case _ => None
      }

    // A field typed `K ->> V` gives its key and value directly; the quoted match below can't bind some keys, e.g. a
    // union of literals. It matches a subtype of `K ->> V`, e.g. an intersection around it, once `->>` is replaced
    // with a concrete class, since matches on an opaque type like `->>` never succeed
    def keyValue(head: TypeRepr): (TypeRepr, TypeRepr) =
      labelledArgs(head).getOrElse {
        val concreteHead = concrete(head.dealias)
        concreteHead.asType match {
          case '[ConcreteLabelled[k, v]] => (unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v]))
          case _ => report.errorAndAbort(s"Unexpected type: ${display(concreteHead)}")
        }
      }

    def stringKey(t: TypeRepr): Option[String] =
      labelledArgs(t).flatMap {
        case (ConstantType(StringConstant(k)), _) => Some(k)
        case _ => None
      }

    // `->>` is invariant in its key, so a field keyed by a different string literal can never satisfy `Cmp[field, F]`
    val fKey = stringKey(fTpe)

    def cmpHolds(head: TypeRepr): Boolean =
      !fKey.exists(fk => stringKey(head).exists(_ != fk)) &&
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          case _ => false
        })

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
        case '[::[head, tail]] if cmpHolds(TypeRepr.of[head]) =>
          val (key, value) = keyValue(TypeRepr.of[head])
          found(revInit, key, value, TypeRepr.of[tail], idx)
        // Checked before `head :: tail`, which `Nothing` also matches, with a `Nothing` tail that would never end the walk
        case _ if TypeRepr.of[RestT] <:< TypeRepr.of[HNil] =>
          report.errorAndAbort(s"Failed to find field ${display(fTpe)} in record ${display(TypeRepr.of[T])}")
        case '[::[head, tail]] =>
          go[tail](TypeRepr.of[head] :: revInit, idx + 1)
      }: @annotation.nowarn("msg=match may not be exhaustive")

    go[T](Nil, 0)
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}
