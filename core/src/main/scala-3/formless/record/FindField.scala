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

  /**
   * Walks a record type looking for the first field `head` such that `Cmp[head, F]` holds.
   *
   * A record whose tail is not statically known (an abstract `L <: HList`) is `Unknown`: it neither contains nor lacks
   * the field, so neither `FindField` nor `FieldAbsent` resolves for it.
   */
  private[formless] final class Lookup[T <: HList: Type, F: Type, Cmp[_, _]: Type](using val q: Quotes) {
    import q.reflect.*

    sealed trait Outcome
    final case class Found(revInit: List[TypeRepr], head: TypeRepr, tail: TypeRepr, index: Int) extends Outcome
    case object Absent extends Outcome
    final case class Unknown(rest: TypeRepr) extends Outcome

    private val labelled = TypeRepr.of[->>]
    private val labelledSym = labelled.typeSymbol
    private val concreteLabelled = TypeRepr.of[ConcreteLabelled]
    private val concreteLabelledSym = concreteLabelled.typeSymbol
    val consSym: Symbol = TypeRepr.of[Any :: HNil].typeSymbol
    private val hnilTpe = TypeRepr.of[HNil]
    private val fTpe = TypeRepr.of[F]
    private val cmpTpe = TypeRepr.of[Cmp]

    private def labelledArgs(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t.dealias match {
        case AppliedType(tycon, List(k, v)) if tycon.typeSymbol == labelledSym => Some((k, v))
        case _ => None
      }

    // Matches up to subtyping (e.g. an intersection around `K ->> V`); matches on the opaque `->>` itself never succeed
    def keyValue(head: TypeRepr): (TypeRepr, TypeRepr) =
      labelledArgs(head).getOrElse {
        val concreteHead = head.dealias.substituteTypes(List(labelledSym), List(concreteLabelled))
        def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
        concreteHead.asType match {
          case '[ConcreteLabelled[k, v]] => (unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v]))
          case _ => report.errorAndAbort(s"Unexpected type: ${concreteHead.show}")
        }
      }

    private def stringKey(t: TypeRepr): Option[String] =
      labelledArgs(t).flatMap {
        case (ConstantType(StringConstant(k)), _) => Some(k)
        case _ => None
      }

    // `->>` is invariant in its key, so a field keyed by a different string literal can never satisfy `Cmp[field, F]`
    private val fKey = stringKey(fTpe)

    private def cmpHolds(head: TypeRepr): Boolean =
      !fKey.exists(fk => stringKey(head).exists(_ != fk)) &&
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          case _ => false
        })

    @annotation.tailrec
    private def go(rest: TypeRepr, revInit: List[TypeRepr], idx: Int): Outcome = {
      val t = rest.dealias.simplified
      t.baseType(consSym) match {
        case AppliedType(_, List(head, tail)) =>
          if (cmpHolds(head)) Found(revInit, head, tail, idx)
          else go(tail, head :: revInit, idx + 1)
        case _ =>
          if (t =:= hnilTpe) Absent else Unknown(t)
      }
    }

    def outcome: Outcome = go(TypeRepr.of[T], Nil, 0)
  }

  private def findFieldInstImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FindField[T, F, Cmp]] = {
    val lookup = Lookup[T, F, Cmp]
    import lookup.q.reflect.*

    val consTpe = lookup.consSym.typeRef
    val instSym = TypeRepr.of[Inst[HNil, Any, =:=, Any, Any, [a] =>> HNil, HNil]].typeSymbol
    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consTpe.appliedTo(List(h, t))

    lookup.outcome match {
      case lookup.Found(revInit, head, tail, idx) =>
        val (key, value) = lookup.keyValue(head)
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
      case lookup.Absent =>
        report.errorAndAbort(s"Failed to find field ${Type.show[F]} in record ${Type.show[T]}")
      case lookup.Unknown(rest) =>
        report.errorAndAbort(s"Cannot find field ${Type.show[F]} in record ${Type.show[T]}: its tail `${rest.show}` is not statically known")
    }
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}

/**
 * Evidence that no field `head` of the record `T` satisfies `Cmp[head, F]`, for a record whose every element is
 * statically known. Unlike `NotGiven[FindField[T, F, Cmp]]`, it does not resolve for a record with an abstract tail,
 * which might contain the field.
 */
private[formless] sealed trait FieldAbsent[T <: HList, F, Cmp[_, _]]

object FieldAbsent {
  val instance: FieldAbsent[HNil, Any, =:=] = new FieldAbsent[HNil, Any, =:=] {}

  private def fieldAbsentImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FieldAbsent[T, F, Cmp]] = {
    val lookup = FindField.Lookup[T, F, Cmp]
    import lookup.q.reflect.*

    lookup.outcome match {
      case lookup.Absent =>
        '{ FieldAbsent.instance.asInstanceOf[FieldAbsent[T, F, Cmp]] }
      case lookup.Found(_, head, _, idx) =>
        report.errorAndAbort(s"Record ${Type.show[T]} has field ${head.show} at index $idx matching ${Type.show[F]}")
      case lookup.Unknown(rest) =>
        report.errorAndAbort(s"Cannot prove ${Type.show[F]} is absent from ${Type.show[T]}: its tail `${rest.show}` is not statically known")
    }
  }

  transparent inline given fieldAbsentInst[T <: HList, F, Cmp[_, _]]: FieldAbsent[T, F, Cmp] = ${ fieldAbsentImpl[T, F, Cmp] }
}
