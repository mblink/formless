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
   * the field.
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

    // `show` throws CyclicReference for a type owned by a method whose result type is still being inferred
    def display(t: TypeRepr): String = scala.util.Try(t.show).getOrElse(t.typeSymbol.name)

    // A type parameter or abstract type member stands for some subtype of its upper bound. A member's bound is taken as
    // seen from its prefix, so `c.H` for `c: C[Int]` and `trait C[A] { type H <: "a" ->> A }` is bounded by `"a" ->> Int`
    private def upperBound(t: TypeRepr): Option[TypeRepr] =
      t.dealias match {
        case tr: TypeRef if tr.typeSymbol.isTypeParam || (tr.typeSymbol.isAbstractType && !tr.typeSymbol.flags.is(Flags.Opaque)) =>
          val bounds = tr.qualifier match {
            case NoPrefix() => scala.util.Try(tr.typeSymbol.tree).toOption.collect { case TypeDef(_, b: TypeBoundsTree) => b.tpe }
            case pre => scala.util.Try(pre.memberType(tr.typeSymbol)).toOption
          }
          bounds.collect { case TypeBounds(_, hi) => hi }
        case _ => None
      }

    // Takes the key and value from an abstract field's upper bound, otherwise from a quoted match up to subtyping
    // (e.g. an intersection around `K ->> V`); `->>` is replaced with a concrete class there since matches on an
    // opaque type never succeed
    def keyValue(head: TypeRepr): (TypeRepr, TypeRepr) =
      upperBound(head).map(keyValue).getOrElse {
        val concreteHead = head.dealias.substituteTypes(List(labelledSym), List(concreteLabelled))
        def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
        concreteHead.asType match {
          case '[ConcreteLabelled[k, v]] => (unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v]))
          case _ => report.errorAndAbort(s"Unexpected type: ${display(concreteHead)}")
        }
      }

    private def stringKey(t: TypeRepr): Option[String] =
      labelledArgs(t).flatMap {
        case (ConstantType(StringConstant(k)), _) => Some(k)
        case _ => None
      }

    // `->>` is invariant in its key, so a field keyed by a different string literal can never satisfy `Cmp[field, F]`
    private val fKey = stringKey(fTpe)

    def cmpHolds(head: TypeRepr): Boolean =
      !fKey.exists(fk => stringKey(head).exists(_ != fk)) &&
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          // More than one witness, e.g. from local givens, still means `Cmp[head, F]` holds
          case _: AmbiguousImplicits => true
          case _ => false
        })

    private def union(a: TypeRepr, b: TypeRepr): TypeRepr = if (a =:= b) a else OrType(a, b)

    // The head and tail of a record type: directly for `H :: T`, through the base type otherwise (an abstract record's
    // upper bound, an intersection, a singleton), and arm by arm for a union of records
    private def consParts(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t match {
        case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol == consSym => Some((head, tail))
        case OrType(a, b) =>
          for {
            (ha, ta) <- consParts(a.dealias.simplified)
            (hb, tb) <- consParts(b.dealias.simplified)
          } yield (union(ha, hb), union(ta, tb))
        case _ =>
          t.baseType(consSym) match {
            case AppliedType(_, List(head, tail)) => Some((head, tail))
            case _ => None
          }
      }

    private def isDirectCons(t: TypeRepr): Boolean =
      t match {
        case AppliedType(tycon, _) => tycon.typeSymbol == consSym
        case OrType(a, b) => isDirectCons(a.dealias.simplified) && isDirectCons(b.dealias.simplified)
        case _ => false
      }

    /** @param expanded the abstract records already walked through their bound, so `type L <: Int :: L` doesn't loop */
    @annotation.tailrec
    private def go(rest: TypeRepr, revInit: List[TypeRepr], idx: Int, expanded: List[TypeRepr]): Outcome = {
      val t = rest.dealias.simplified
      val direct = isDirectCons(t)
      if (!direct && expanded.exists(_ =:= t)) Unknown(t)
      else consParts(t) match {
        case Some((head, tail)) =>
          if (cmpHolds(head)) Found(revInit, head, tail, idx)
          else go(tail, head :: revInit, idx + 1, if (direct) expanded else t :: expanded)
        case None =>
          if (t <:< hnilTpe) Absent
          else t match {
            // A union of records that don't all have another element, e.g. `HNil | (A :: HNil)`: absent only when it's
            // absent from every arm
            case OrType(a, b) => absenceInArms(a, b, revInit, idx, expanded)
            case _ => Unknown(t)
          }
      }
    }

    private def absenceInArms(a: TypeRepr, b: TypeRepr, revInit: List[TypeRepr], idx: Int, expanded: List[TypeRepr]): Outcome =
      go(a, revInit, idx, expanded) match {
        case Absent => go(b, revInit, idx, expanded)
        case other => other
      }

    def outcome: Outcome = go(TypeRepr.of[T], Nil, 0, Nil)
  }

  private def findFieldInstImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FindField[T, F, Cmp]] = {
    val lookup = Lookup[T, F, Cmp]
    import lookup.q.reflect.*

    val consTpe = lookup.consSym.typeRef
    val instSym = TypeRepr.of[Inst[HNil, Any, =:=, Any, Any, [a] =>> HNil, HNil]].typeSymbol
    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consTpe.appliedTo(List(h, t))

    // `Replaced` and `Removed` are built as concrete HList types, not as nested `HList.Append`/`HList.Concat` match types
    def found(revInit: List[TypeRepr], head: TypeRepr, tail: TypeRepr, idx: Int): Expr[FindField[T, F, Cmp]] = {
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
    }

    // `revInit` holds the fields before `RestT`, nearest first
    def go[RestT <: HList: Type](revInit: List[TypeRepr], idx: Int): Expr[FindField[T, F, Cmp]] =
      Type.of[RestT] match {
        case '[::[head, tail]] if lookup.cmpHolds(TypeRepr.of[head]) =>
          found(revInit, TypeRepr.of[head], TypeRepr.of[tail], idx)
        // Checked before `head :: tail`, which `Nothing` also matches, with a `Nothing` tail that would never end the walk
        case _ if TypeRepr.of[RestT] <:< TypeRepr.of[HNil] =>
          report.errorAndAbort(s"Failed to find field ${lookup.display(TypeRepr.of[F])} in record ${lookup.display(TypeRepr.of[T])}")
        case '[::[head, tail]] =>
          go[tail](TypeRepr.of[head] :: revInit, idx + 1)
      }: @annotation.nowarn("msg=match may not be exhaustive")

    go[T](Nil, 0)
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}

/**
 * Evidence that no field `head` of the record `T` satisfies `Cmp[head, F]`, for a record whose every element is
 * statically known. Unlike `NotGiven[FindField[T, F, Cmp]]`, it does not resolve for a record with an abstract tail,
 * which might contain the field, or for one with a field that satisfies `Cmp[head, F]` but that `FindField` can't use.
 */
private[formless] opaque type FieldAbsent[T <: HList, F, Cmp[_, _]] = Unit

object FieldAbsent {
  private def fieldAbsentImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FieldAbsent[T, F, Cmp]] = {
    val lookup = FindField.Lookup[T, F, Cmp]
    import lookup.q.reflect.*

    lookup.outcome match {
      case lookup.Absent =>
        '{ ().asInstanceOf[FieldAbsent[T, F, Cmp]] }
      case lookup.Found(_, head, _, idx) =>
        report.errorAndAbort(
          s"Record ${lookup.display(TypeRepr.of[T])} has field ${lookup.display(head)} at index $idx matching ${lookup.display(TypeRepr.of[F])}"
        )
      case lookup.Unknown(rest) =>
        report.errorAndAbort(
          s"Cannot prove ${lookup.display(TypeRepr.of[F])} is absent from ${lookup.display(TypeRepr.of[T])}: its tail `${lookup.display(rest)}` is not statically known"
        )
    }
  }

  transparent inline given fieldAbsentInst[T <: HList, F, Cmp[_, _]]: FieldAbsent[T, F, Cmp] = ${ fieldAbsentImpl[T, F, Cmp] }
}
