package formless
package hlist

import scala.quoted.*

/**
 * The result types of an HList operation `Op` on an element `=:=` to `U` of `T`, with the index it acts on: the first
 * such element, or for `SplitRight` and `ReverseSplitRight` the last. `A` and `B` are built directly by a macro, rather
 * than as `HList.Take`, `HList.Drop`, `HList.Reverse` or nested `HList.Append` match types for the compiler to reduce.
 */
private[formless] sealed trait ElemOp[T <: HList, U, Op] {
  type A
  type B
  val index: Int
}

object ElemOp {
  /** `A` is `T` without the element, keeping each arm of a union of lists separate; `B` is unused */
  sealed trait Remove
  /** `A` is `T` with `V` in place of the element; `B` is unused */
  sealed trait Replace[V]
  /** `A` is the elements before the first match; `B` is the match and the elements after it */
  sealed trait SplitLeft
  /** `A` is the elements before the first match, reversed; `B` is the match and the elements after it */
  sealed trait ReverseSplitLeft
  /** `A` is the elements before the last match followed by `U`; `B` is the elements after it */
  sealed trait SplitRight
  /** `A` is `U` followed by the elements before the last match, reversed; `B` is the elements after it */
  sealed trait ReverseSplitRight

  final class Inst[T <: HList, U, Op, A0, B0](val index: Int) extends ElemOp[T, U, Op] {
    final type A = A0
    final type B = B0
  }

  private def elemOpImpl[T <: HList: Type, U: Type, Op: Type](using Quotes): Expr[ElemOp[T, U, Op]] =
    formless.record.FindField.guarded("look up an HList element")(elemOpImpl0[T, U, Op])

  private def elemOpImpl0[T <: HList: Type, U: Type, Op: Type](using Quotes): Expr[ElemOp[T, U, Op]] = {
    val lookup = formless.record.FindField.Lookup[T, U, =:=]
    import lookup.q.reflect.*

    val hnil = TypeRepr.of[HNil]
    val u = TypeRepr.of[U]
    def list(elems: List[TypeRepr]): TypeRepr = elems.foldRight(hnil)(lookup.cons)
    def inst(a: TypeRepr, b: TypeRepr, idx: Int): Expr[ElemOp[T, U, Op]] =
      ((a.asType, b.asType): @unchecked) match {
        case ('[a], '[b]) => '{ new Inst[T, U, Op, a, b](${ Expr(idx) }) }
      }

    Type.of[Op] match {
      case '[Remove] =>
        val f = lookup.foundOrAbort
        // `reinsert` puts the element back and calls the result a `T`, which only holds when the removed type describes
        // `T`'s own elements, not those of an abstract `T`'s upper bound, which `T` may narrow
        if (f.bounded) report.errorAndAbort(
          s"Cannot remove ${lookup.display(u)} from ${lookup.display(TypeRepr.of[T])}: it was found in the upper bound of an abstract list, so it couldn't be reinserted"
        )
        inst(lookup.removedByArm(f.index), hnil, f.index)
      case '[Replace[v]] =>
        val f = lookup.foundOrAbort
        inst(lookup.replaced(f, TypeRepr.of[v]), hnil, f.index)
      case '[SplitLeft] =>
        val f = lookup.foundOrAbort
        inst(list(f.revInit.reverse), lookup.restAt(f.index), f.index)
      case '[ReverseSplitLeft] =>
        val f = lookup.foundOrAbort
        inst(list(f.revInit), lookup.restAt(f.index), f.index)
      case '[SplitRight] =>
        val f = lookup.lastFoundOrAbort
        inst(list((u :: f.revInit).reverse), f.tail, f.index)
      case '[ReverseSplitRight] =>
        val f = lookup.lastFoundOrAbort
        inst(lookup.cons(u, list(f.revInit)), f.tail, f.index)
      case _ =>
        report.errorAndAbort("Unexpected HList operation: expected Remove, Replace, or one of the splits")
    }
  }

  transparent inline given elemOpInst[T <: HList, U, Op]: ElemOp[T, U, Op] = ${ elemOpImpl[T, U, Op] }
}
