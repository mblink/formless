package formless
package hlist

import formless.record.Lookup
import scala.quoted.*

/**
 * The result types of an HList operation `Op` on an element `=:=` to `U` of `T`, with the index it acts on: the first
 * such element, or for `SplitRight` and `ReverseSplitRight` the last. `A` and `B` are built directly by a macro, rather
 * than as `HList.Take`, `HList.Drop`, `HList.Reverse` or nested `HList.Append` match types for the compiler to reduce.
 */
@scala.annotation.implicitNotFound("Cannot find an element of ${T} of type ${U}")
private[formless] sealed trait ElemOp[T <: HList, U, Op <: ElemOp.Op] extends Serializable {
  type A <: HList
  type B <: HList
  val index: Int
}

object ElemOp {
  sealed trait Op
  /** `A` is `T` without the element, keeping each arm of a union of lists separate; `B` is unused */
  sealed trait Remove extends Op
  /** `A` is the elements before the first match; `B` is the match and the elements after it */
  sealed trait SplitLeft extends Op
  /** `A` is the elements before the first match, reversed; `B` is the match and the elements after it */
  sealed trait ReverseSplitLeft extends Op
  /** `A` is the elements before the last match followed by `U`; `B` is the elements after it */
  sealed trait SplitRight extends Op
  /** `A` is `U` followed by the elements before the last match, reversed; `B` is the elements after it */
  sealed trait ReverseSplitRight extends Op

  // Built only by the macro below, which proves `index` is the element of `T` that `A0` and `B0` were built around
  private[formless] final class Inst[T <: HList, U, Op <: ElemOp.Op, A0 <: HList, B0 <: HList](val index: Int) extends ElemOp[T, U, Op] {
    final type A = A0
    final type B = B0
  }

  // Scala 3.3 quote patterns cannot bind the HList-bounded result arguments of these constructors.
  private[hlist] trait Constructor {
    def apply(q: Quotes): (q.reflect.TypeRepr, List[q.reflect.TypeRepr])
  }

  private def elemOpImpl[T <: HList: Type, U: Type, Op <: ElemOp.Op: Type](using Quotes): Expr[ElemOp[T, U, Op]] =
    materialize[T, U, Op, ElemOp[T, U, Op]] { q =>
      import q.reflect.*
      (TypeRepr.of[Inst[HNil, Any, Remove, HNil, HNil]], List(TypeRepr.of[T], TypeRepr.of[U], TypeRepr.of[Op]))
    }

  private[hlist] def materialize[T <: HList: Type, U: Type, Op <: ElemOp.Op: Type, E: Type](
    constructor: Constructor,
  )(using Quotes): Expr[E] =
    Lookup.guarded("look up an HList element")(elemResult[T, U, Op, E](constructor))

  private def elemResult[T <: HList: Type, U: Type, Op <: ElemOp.Op: Type, E: Type](
    constructor: Constructor,
  )(using Quotes): Either[Lookup.LookupError, Expr[E]] = {
    val lookup = Lookup.Walk[T, U, =:=](Lookup.Target.Element)
    import lookup.q.reflect.*

    val hnil = TypeRepr.of[HNil]
    val u = TypeRepr.of[U]
    def list(reversed: List[TypeRepr]): TypeRepr = reversed.foldLeft(hnil)((tail, head) => lookup.cons(head, tail))
    def inst(f: lookup.Found, a: TypeRepr, b: TypeRepr): Expr[E] = {
      val (cls, inputs) = constructor(lookup.q)
      val symbol = cls.typeSymbol
      val arguments = inputs ::: List(a, b)
      New(Inferred(symbol.typeRef.appliedTo(arguments)))
        .select(symbol.primaryConstructor)
        .appliedToTypes(arguments)
        .appliedTo(Literal(IntConstant(f.index)))
        .asExprOf[E]
    }

    Type.of[Op] match {
      case '[Remove] =>
        // `reinsert` puts the element back and calls the result a `T`, so the removed type is built only where it can
        for {
          f <- lookup.found
          removed <- lookup.removedReinsertably(f)
        } yield inst(f, removed, hnil)
      case '[SplitLeft] => lookup.found.map(f => inst(f, list(f.revInit), f.rest))
      case '[ReverseSplitLeft] => lookup.found.map(f => inst(f, list(f.revInit.reverse), f.rest))
      case '[SplitRight] => lookup.lastFound.map(f => inst(f, list(u :: f.revInit), f.tail))
      case '[ReverseSplitRight] => lookup.lastFound.map(f => inst(f, lookup.cons(u, list(f.revInit.reverse)), f.tail))
      case _ => Left(Lookup.LookupError.AbstractOperation(Type.show[Op]))
    }
  }

  transparent inline given elemOpInst[T <: HList, U, Op <: ElemOp.Op]: ElemOp[T, U, Op] = ${ elemOpImpl[T, U, Op] }
}
