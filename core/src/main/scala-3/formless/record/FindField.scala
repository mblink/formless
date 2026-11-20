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
      upperBound(head).map(keyValue).orElse(head.dealias match {
        // e.g. `("a" ->> Int) | ("a" ->> String)`, from a union of records; its arms may be aliases `dealias` leaves alone
        case OrType(a, b) =>
          val ((ka, va), (kb, vb)) = (keyValue(a), keyValue(b))
          Some((union(ka, kb), union(va, vb)))
        // e.g. `("a" ->> Int) & ("b" ->> String)` looked up by `"b"`: the quoted match below would pick a key and value
        // from neither conjunct in particular, so take them from the conjuncts keyed like `F`
        case and: AndType => intersectionKeyValue(and)
        case _ => None
      }).getOrElse {
        val concreteHead = head.dealias.substituteTypes(List(labelledSym), List(concreteLabelled))
        def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
        concreteHead.asType match {
          case '[ConcreteLabelled[k, v]] => (unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v]))
          case _ => report.errorAndAbort(s"Unexpected type: ${display(concreteHead)}")
        }
      }

    private def union(a: TypeRepr, b: TypeRepr): TypeRepr = if (a =:= b) a else OrType(a, b)

    private def conjuncts(t: TypeRepr): List[TypeRepr] =
      t.dealias match {
        case AndType(a, b) => conjuncts(a) ++ conjuncts(b)
        case other => List(other)
      }

    // `F`'s key, when `F` is a field whose key is statically known
    private lazy val fFieldKey: Option[TypeRepr] = labelledKey(fTpe).filter(isGround)

    private def intersectionKeyValue(t: AndType): Option[(TypeRepr, TypeRepr)] =
      fFieldKey.flatMap { fk =>
        conjuncts(t)
          .flatMap(c => labelledArgs(c).orElse(upperBound(c).flatMap(labelledArgs)))
          .filter(_._1 =:= fk)
          .reduceOption((x, y) => (x._1, if (x._2 =:= y._2) x._2 else AndType(x._2, y._2)))
      }

    // The value of a field's literal key, e.g. `"a"`, `1`, `1L` or `'1'`
    private def literalKey(t: TypeRepr): Option[Any] =
      labelledArgs(t).flatMap { case (k, _) =>
        k.dealias match {
          case ConstantType(c) if c.value != null => Some(c.value)
          case _ => None
        }
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
        case mt: MethodType => mt.paramTypes.forall(isGround) && isGround(mt.resType)
        // A singleton of an abstract type, e.g. `x.type` for `x: T`, could become a subtype of anything `T` could be, and
        // so could `b.x.type` for `b: B` once `B` is known
        case tr: TermRef => isGround(tr.qualifier) && isGround(tr.widen)
        case _: ConstantType | _: ThisType | _: NoPrefix => true
        case _ => false
      }

    private def labelledKey(t: TypeRepr): Option[TypeRepr] =
      labelledArgs(t).orElse(upperBound(t).flatMap(labelledArgs)).map(_._1)

    // An abstract element bounded by `"a" ->> V` may still be `("a" ->> V) & ("b" ->> W)`, a subtype of `"b" ->> Any`, so
    // its bound's key rules out other keys only under `=:=`. Bounding `F` instead is fine: `head <: F <: ("b" ->> W)`
    // and `head =:= F` both need `head`'s key to be `"b"`.
    private val cmpIsEq = cmpTpe.typeSymbol == TypeRepr.of[=:=].typeSymbol

    // Whether `Cmp[head, F]` could hold once the abstract types in `head` or `F` are known. A `bounded` head was read
    // off the upper bound of an abstract record, so the element it stands for may be any subtype of it.
    private def mightMatch(head: TypeRepr, bounded: Boolean): Boolean = {
      val headKey =
        if (cmpIsEq) labelledKey(head)
        else if (bounded) None
        else labelledArgs(head).map(_._1)
      (headKey, labelledKey(fTpe)) match {
        case (Some(hk), Some(fk)) if isGround(hk) && isGround(fk) && !(hk =:= fk) => false
        case _ => bounded || !(isGround(head) && isGround(fTpe))
      }
    }

    // `->>` is invariant in its key, so a field keyed by a different literal can never satisfy `Cmp[field, F]`. Literals
    // of different types differ even when `==` says otherwise, as `1 == 1L` and `49 == '1'` do.
    private val fKey = literalKey(fTpe)

    private def keyMismatch(head: TypeRepr): Boolean =
      fKey.exists(fk => literalKey(head).exists(hk => hk.getClass != fk.getClass || hk != fk))

    private def cmpHolds(head: TypeRepr): Boolean =
      !keyMismatch(head) &&
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          // More than one witness, e.g. from local givens, still means `Cmp[head, F]` holds
          case _: AmbiguousImplicits => true
          case _ => false
        })

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

    /**
     * @param decided whether proving absence, so every element must be ruled out, and a union of records may be split
     *                into arms that end at different points
     * @param bounded whether the elements so far were read off an abstract record's upper bound, so each stands for
     *                some subtype of itself
     * @param expanded the abstract records already walked through their bound, so `type L <: Int :: L` doesn't loop
     */
    @annotation.tailrec
    private def go(
      rest: TypeRepr,
      revInit: List[TypeRepr],
      idx: Int,
      decided: Boolean,
      bounded: Boolean,
      expanded: List[TypeRepr],
    ): Outcome = {
      // Most tails are already `H :: T`, which needs no normalizing
      val t = rest match {
        case AppliedType(tycon, _) if tycon.typeSymbol == consSym => rest
        case _ => rest.dealias.simplified
      }
      val direct = isDirectCons(t)
      if (!direct && expanded.exists(_ =:= t)) Unknown(t)
      else consParts(t) match {
        case Some((head, tail)) =>
          val nowBounded = bounded || !(direct || isGround(t))
          if (cmpHolds(head)) Found(revInit, head, tail, idx)
          else if (decided && mightMatch(head, nowBounded)) Undecided(head, idx)
          else go(tail, head :: revInit, idx + 1, decided, nowBounded, if (direct) expanded else t :: expanded)
        case None =>
          if (t <:< hnilTpe) Absent
          else t match {
            // A union of records that don't all have another element, e.g. `HNil | (A :: HNil)`: absent only when it's
            // absent from every arm
            case OrType(a, b) if decided => absenceInArms(a, b, revInit, idx, bounded, expanded)
            case _ => Unknown(t)
          }
      }
    }

    private def absenceInArms(
      a: TypeRepr,
      b: TypeRepr,
      revInit: List[TypeRepr],
      idx: Int,
      bounded: Boolean,
      expanded: List[TypeRepr],
    ): Outcome =
      go(a, revInit, idx, decided = true, bounded, expanded) match {
        case Absent => go(b, revInit, idx, decided = true, bounded, expanded)
        case other => other
      }

    def outcome: Outcome = go(TypeRepr.of[T], Nil, 0, decided = false, bounded = false, expanded = Nil)

    /** Like `outcome`, but `Undecided` at the first element that doesn't match yet might once its types are known */
    def absence: Outcome = go(TypeRepr.of[T], Nil, 0, decided = true, bounded = false, expanded = Nil)
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
