package formless
package hlist

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

  // Kept for the deprecated methods that still take a `FindField`; it walks the list as `ElemOp` does, so it is refused
  // cleanly wherever `ElemOp` is
  private def findFieldInstImpl[T <: HList: Type, F: Type](using q: Quotes): Expr[FindField[T, F]] =
    formless.record.FindField.guarded("look up an HList element")(findFieldInstImpl0[T, F])

  private def findFieldInstImpl0[T <: HList: Type, F: Type](using Quotes): Expr[FindField[T, F]] = {
    val lookup = formless.record.FindField.Lookup[T, F, =:=]
    import lookup.q.reflect.*

    val f = lookup.foundOrAbort
    // Its users include `Remove.reinsert`, which only holds for `T`'s own elements, as with `ElemOp.Remove`
    if (f.bounded) report.errorAndAbort(
      s"Cannot find ${lookup.display(TypeRepr.of[F])} in ${lookup.display(TypeRepr.of[T])}: it was found in the upper bound of an abstract list"
    )
    val prefix = f.revInit.reverse
    def list(elems: List[TypeRepr], end: TypeRepr): TypeRepr = elems.foldRight(end)(lookup.cons)
    val replaced = TypeLambda(List("a"), _ => List(TypeBounds.empty), tl => list(prefix, lookup.cons(tl.param(0), f.tail)))
    val idx = ConstantType(IntConstant(f.index))
    val instSym = TypeRepr.of[Inst[HNil, Any, Any, HNil, HNil, [a] =>> HNil, HNil, 0]].typeSymbol
    val targs = List(TypeRepr.of[T], TypeRepr.of[F], f.head, list(prefix, TypeRepr.of[HNil]), f.tail, replaced, lookup.removed(f), idx)
    New(Inferred(instSym.typeRef.appliedTo(targs)))
      .select(instSym.primaryConstructor)
      .appliedToTypes(targs)
      .appliedTo(Literal(IntConstant(f.index)))
      .asExprOf[FindField[T, F]]
  }

  transparent inline given findFieldInst[T <: HList, F]: FindField[T, F] = ${ findFieldInstImpl[T, F] }
}
