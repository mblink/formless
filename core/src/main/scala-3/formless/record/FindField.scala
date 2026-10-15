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
    final case class Undecided(head: TypeRepr, index: Int) extends Outcome

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

    // A type parameter or abstract type member stands for some subtype of its upper bound
    private def upperBound(t: TypeRepr): Option[TypeRepr] =
      t.dealias match {
        case tr: TypeRef if tr.typeSymbol.isTypeParam || (tr.typeSymbol.isAbstractType && !tr.typeSymbol.flags.is(Flags.Opaque)) =>
          scala.util.Try(tr.typeSymbol.tree).toOption.collect { case TypeDef(_, b: TypeBoundsTree) => b.hi.tpe }
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

    // Free of type parameters and abstract type members, so a failed `Cmp` check against it is final
    private def isGround(t: TypeRepr): Boolean =
      t.dealias match {
        case tr: TypeRef =>
          !(tr.typeSymbol.isTypeParam || (tr.typeSymbol.isAbstractType && !tr.typeSymbol.flags.is(Flags.Opaque))) && isGround(tr.qualifier)
        case AppliedType(tycon, args) => isGround(tycon) && args.forall(isGround)
        case AndType(a, b) => isGround(a) && isGround(b)
        case OrType(a, b) => isGround(a) && isGround(b)
        case AnnotatedType(u, _) => isGround(u)
        case Refinement(parent, _, info) => isGround(parent) && isGround(info)
        case TypeBounds(lo, hi) => isGround(lo) && isGround(hi)
        case ByNameType(u) => isGround(u)
        case _: ConstantType | _: TermRef | _: ThisType | _: NoPrefix => true
        case _ => false
      }

    private def labelledKey(t: TypeRepr): Option[TypeRepr] =
      labelledArgs(t).orElse(upperBound(t).flatMap(labelledArgs)).map(_._1)

    // Whether `Cmp[head, F]` could hold once the abstract types in `head` or `F` are known
    private def mightMatch(head: TypeRepr): Boolean =
      (labelledKey(head), labelledKey(fTpe)) match {
        case (Some(hk), Some(fk)) if isGround(hk) && isGround(fk) && !(hk =:= fk) => false
        case _ => !(isGround(head) && isGround(fTpe))
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
    private def go(rest: TypeRepr, revInit: List[TypeRepr], idx: Int, decided: Boolean): Outcome = {
      val t = rest.dealias.simplified
      t.baseType(consSym) match {
        case AppliedType(_, List(head, tail)) =>
          if (cmpHolds(head)) Found(revInit, head, tail, idx)
          else if (decided && mightMatch(head)) Undecided(head, idx)
          else go(tail, head :: revInit, idx + 1, decided)
        case _ =>
          if (t <:< hnilTpe) Absent else Unknown(t)
      }
    }

    def outcome: Outcome = go(TypeRepr.of[T], Nil, 0, decided = false)

    /** Like `outcome`, but `Undecided` at the first element that doesn't match yet might once its types are known */
    def absence: Outcome = go(TypeRepr.of[T], Nil, 0, decided = true)
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

    lookup.outcome match {
      case lookup.Found(revInit, head, tail, idx) =>
        found(revInit, head, tail, idx)
      case lookup.Absent =>
        report.errorAndAbort(s"Failed to find field ${lookup.display(TypeRepr.of[F])} in record ${lookup.display(TypeRepr.of[T])}")
      case lookup.Unknown(rest) =>
        report.errorAndAbort(
          s"Cannot find field ${lookup.display(TypeRepr.of[F])} in record ${lookup.display(TypeRepr.of[T])}: its tail `${lookup.display(rest)}` is not statically known"
        )
      case lookup.Undecided(head, idx) =>
        report.errorAndAbort(s"Unexpected undecided element ${lookup.display(head)} at index $idx")
    }
  }

  transparent inline given findFieldInst[T <: HList, F, Cmp[_, _]]: FindField[T, F, Cmp] = ${ findFieldInstImpl[T, F, Cmp] }
}

/**
 * Evidence that no field `head` of the record `T` satisfies `Cmp[head, F]`, whatever its abstract types turn out to be.
 * Unlike `NotGiven[FindField[T, F, Cmp]]`, it does not resolve for a record with an abstract tail, with an element that
 * might match once its type parameters are known, or with a field that satisfies `Cmp[head, F]` but that `FindField`
 * can't use.
 */
private[formless] opaque type FieldAbsent[T <: HList, F, Cmp[_, _]] = Unit

object FieldAbsent {
  private def fieldAbsentImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FieldAbsent[T, F, Cmp]] = {
    val lookup = FindField.Lookup[T, F, Cmp]
    import lookup.q.reflect.*

    lookup.absence match {
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
      case lookup.Undecided(head, idx) =>
        report.errorAndAbort(
          s"Cannot prove ${lookup.display(TypeRepr.of[F])} is absent from ${lookup.display(TypeRepr.of[T])}: element `${lookup.display(head)}` at index $idx might match"
        )
    }
  }

  transparent inline given fieldAbsentInst[T <: HList, F, Cmp[_, _]]: FieldAbsent[T, F, Cmp] = ${ fieldAbsentImpl[T, F, Cmp] }
}
