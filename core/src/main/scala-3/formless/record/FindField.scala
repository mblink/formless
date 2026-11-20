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

    private enum Mode { case Find, Prove, Update }

    sealed trait Outcome
    /** `bounded` when `head` was read off an abstract record's upper bound, so the element may be narrower */
    final case class Found(revInit: List[TypeRepr], head: TypeRepr, tail: TypeRepr, index: Int)(val bounded: Boolean) extends Outcome
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
      keyValueOpt(head).getOrElse(report.errorAndAbort(s"Unexpected type: ${display(head.dealias)}"))

    def keyValueOpt(head: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      upperBound(head) match {
        case Some(bound) => keyValueOpt(bound)
        case None =>
          head.dealias match {
            // e.g. `("a" ->> Int) | ("a" ->> String)`, from a union of records; its arms may be aliases `dealias` leaves
            // alone
            case OrType(a, b) =>
              for {
                (ka, va) <- keyValueOpt(a)
                (kb, vb) <- keyValueOpt(b)
              } yield (union(ka, kb), union(va, vb))
            // e.g. `("a" ->> Int) & ("b" ->> String)` looked up by `"b"`: the quoted match below would pick a key and
            // value from neither conjunct in particular, so take them from the conjuncts keyed like `F`
            case and: AndType => intersectionKeyValue(and).orElse(matchedKeyValue(head))
            case _ => matchedKeyValue(head)
          }
      }

    // Aliases inside intersections and unions too, e.g. `A` in `A & Serializable` for `type A = "a" ->> Int`, which
    // `substituteTypes` would otherwise not see through
    private def deepDealias(t: TypeRepr): TypeRepr =
      t.dealias match {
        case AndType(a, b) => AndType(deepDealias(a), deepDealias(b))
        case OrType(a, b) => OrType(deepDealias(a), deepDealias(b))
        case AnnotatedType(u, _) => deepDealias(u)
        case other => other
      }

    private def matchedKeyValue(head: TypeRepr): Option[(TypeRepr, TypeRepr)] = {
      val concreteHead = deepDealias(head).substituteTypes(List(labelledSym), List(concreteLabelled))
      def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
      concreteHead.asType match {
        case '[ConcreteLabelled[k, v]] => Some((unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v])))
        case _ => None
      }
    }

    private def union(a: TypeRepr, b: TypeRepr): TypeRepr = if (a =:= b) a else OrType(a, b)

    private def conjuncts(t: TypeRepr): List[TypeRepr] =
      t.dealias match {
        case AndType(a, b) => conjuncts(a) ++ conjuncts(b)
        case other => List(other)
      }

    // `F`'s key, when `F` is a field whose key is statically known
    private lazy val fFieldKey: Option[TypeRepr] = fLabelledKey.filter(isGround)

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
    private def isGround(t: TypeRepr): Boolean = isGround(t, Set.empty)

    /** @param opaques the opaque types whose bounds are being checked, so a bound that refers to its own type stops */
    private def isGround(t: TypeRepr, opaques: Set[Symbol]): Boolean = {
      def ground(t: TypeRepr): Boolean = isGround(t, opaques)
      t.dealias match {
        case tr: TypeRef =>
          val sym = tr.typeSymbol
          val opaque = sym.flags.is(Flags.Opaque)
          !(sym.isTypeParam || (sym.isAbstractType && !opaque)) && ground(tr.qualifier) &&
            // An opaque type stands for some subtype of its bound, which may itself be abstract, e.g. `opaque type X <: T`
            // in a class with a type parameter `T`
            (!opaque || sym == labelledSym || opaques(sym) || opaqueBoundGround(tr, opaques + sym))
        case AppliedType(tycon, args) => ground(tycon) && args.forall(ground)
        case AndType(a, b) => ground(a) && ground(b)
        case OrType(a, b) => ground(a) && ground(b)
        case AnnotatedType(u, _) => ground(u)
        case Refinement(parent, _, info) => ground(parent) && ground(info)
        case TypeBounds(lo, hi) => ground(lo) && ground(hi)
        case ByNameType(u) => ground(u)
        case mt: MethodType => mt.paramTypes.forall(ground) && ground(mt.resType)
        // A type lambda's own parameters, e.g. in `Payload[[X] =>> Either[Int, X]]`, are bound within it
        case tl: TypeLambda => tl.paramBounds.forall(ground) && ground(tl.resType)
        case pt: PolyType => pt.paramBounds.forall(ground) && ground(pt.resType)
        case _: ParamRef => true
        case RecursiveType(u) => ground(u)
        case _: RecursiveThis => true
        // A singleton of an abstract type, e.g. `x.type` for `x: T`, could become a subtype of anything `T` could be, and
        // so could `b.x.type` for `b: B` once `B` is known. A method parameter's singleton, e.g. `x.type` for
        // `x: String`, could become the same path as another one once the method is applied.
        case tr: TermRef => !tr.termSymbol.flags.is(Flags.Param) && ground(tr.qualifier) && ground(tr.widen)
        case _: ConstantType | _: ThisType | _: NoPrefix => true
        case _ => false
      }
    }

    private def opaqueBoundGround(tr: TypeRef, opaques: Set[Symbol]): Boolean =
      scala.util.Try(tr.qualifier.memberType(tr.typeSymbol)).toOption.forall {
        case TypeBounds(_, hi) => isGround(hi, opaques)
        case other => isGround(other, opaques)
      }

    private def labelledKey(t: TypeRepr): Option[TypeRepr] =
      labelledArgs(t).orElse(upperBound(t).flatMap(labelledArgs)).map(_._1)

    // An abstract element bounded by `"a" ->> V` may still be `("a" ->> V) & ("b" ->> W)`, a subtype of `"b" ->> Any`, so
    // its bound's key rules out other keys only under `=:=`. Bounding `F` instead is fine: `head <: F <: ("b" ->> W)`
    // and `head =:= F` both need `head`'s key to be `"b"`.
    private val cmpIsEq = cmpTpe.typeSymbol == TypeRepr.of[=:=].typeSymbol

    private lazy val fLabelledKey = labelledKey(fTpe)

    // Whether `Cmp[head, F]` could hold once the abstract types in `head` or `F` are known. A `bounded` head was read
    // off the upper bound of an abstract record, so the element it stands for may be any subtype of it.
    private def mightMatch(head: TypeRepr, bounded: Boolean): Boolean = {
      val headKey =
        if (cmpIsEq) labelledKey(head)
        else if (bounded) None
        else labelledArgs(head).map(_._1)
      (headKey, fLabelledKey) match {
        case (Some(hk), Some(fk)) if isGround(hk) && isGround(fk) && !(hk =:= fk) => false
        // `head =:= F` needs `F` within `head`'s upper bound, e.g. never for `H <: String` and `Int`
        case _ if cmpIsEq && upperBound(head).exists(hi => isGround(hi) && isGround(fTpe) && !(fTpe <:< hi)) => false
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
     * @param mode `Find` looks for the first match; `Prove` also stops, `Undecided`, at the first element that might
     *             match once its types are known, and splits a union of records into arms that end at different points;
     *             `Update` looks for the first match like `Find`, but ends `Undecided` rather than `Absent` when an
     *             element it walked past might have matched
     * @param pending in `Update` mode, the first element walked past that might have matched
     * @param bounded whether the elements so far were read off an abstract record's upper bound, so each stands for
     *                some subtype of itself
     * @param expanded the abstract records already walked through their bound, so `type L <: Int :: L` doesn't loop
     */
    @annotation.tailrec
    private def go(
      rest: TypeRepr,
      revInit: List[TypeRepr],
      idx: Int,
      mode: Mode,
      pending: Option[Undecided],
      bounded: Boolean,
      expanded: List[TypeRepr],
    ): Outcome = {
      // Most tails are already `H :: T`, which needs no normalizing
      val t = normalize(rest)
      val direct = isDirectCons(t)
      if (!direct && expandedBefore(t, expanded)) Unknown(t)
      else consParts(t) match {
        case Some((head, tail)) =>
          val nowBounded = bounded || !(direct || isGround(t))
          lazy val undecided = mightMatch(head, nowBounded)
          if (cmpHolds(head)) Found(revInit, head, tail, idx)(nowBounded)
          else if (mode == Mode.Prove && undecided) Undecided(head, idx)
          else {
            val nowPending = if (mode == Mode.Update && pending.isEmpty && undecided) Some(Undecided(head, idx)) else pending
            go(tail, head :: revInit, idx + 1, mode, nowPending, nowBounded, if (direct) expanded else t :: expanded)
          }
        case None =>
          if (t <:< hnilTpe) pending.getOrElse(Absent)
          else t match {
            // A union of records that don't all have another element, e.g. `HNil | (A :: HNil)`: absent only when it's
            // absent from every arm
            case OrType(a, b) if mode == Mode.Prove => absenceInArms(a, b, revInit, idx, bounded, expanded)
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
      go(a, revInit, idx, Mode.Prove, None, bounded, expanded) match {
        case Absent => go(b, revInit, idx, Mode.Prove, None, bounded, expanded)
        case other => other
      }

    private def start(mode: Mode): Outcome = go(TypeRepr.of[T], Nil, 0, mode, None, bounded = false, expanded = Nil)

    def outcome: Outcome = start(Mode.Find)

    /** The field `outcome` finds, aborting with why when there's none */
    def foundOrAbort: Found =
      outcome match {
        case f: Found => f
        case Absent =>
          report.errorAndAbort(s"Failed to find field ${display(TypeRepr.of[F])} in record ${display(TypeRepr.of[T])}")
        case Unknown(rest) =>
          report.errorAndAbort(
            s"Cannot find field ${display(TypeRepr.of[F])} in record ${display(TypeRepr.of[T])}: its tail `${display(rest)}` is not statically known"
          )
        case Undecided(head, idx) =>
          report.errorAndAbort(s"Unexpected undecided element ${display(head)} at index $idx")
      }

    // The abstract type a record type stands for, e.g. `L` for `c.L[Int]`, to stop a walk through bounds that refer to
    // themselves even as their type arguments change, e.g. `type L[A] <: Int :: L[List[A]]`
    private def abstractSymbol(t: TypeRepr): Option[Symbol] = {
      val sym = t match {
        case AppliedType(tycon, _) => tycon.typeSymbol
        case _ => t.typeSymbol
      }
      Option.when(!sym.isNoSymbol && (sym.isTypeParam || (sym.isAbstractType && !sym.flags.is(Flags.Opaque))))(sym)
    }

    private def expandedBefore(t: TypeRepr, expanded: List[TypeRepr]): Boolean = {
      val sym = abstractSymbol(t)
      expanded.exists(e => e =:= t || (sym.nonEmpty && abstractSymbol(e) == sym))
    }

    private def normalize(t: TypeRepr): TypeRepr =
      t match {
        case AppliedType(tycon, _) if tycon.typeSymbol == consSym => t
        case _ => t.dealias.simplified
      }

    /** The rest of `T` from index `idx` on, as spelled in `T`, e.g. `tail.type` rather than its elements */
    def restAt(idx: Int): TypeRepr =
      (0 until idx).foldLeft(TypeRepr.of[T])((t, _) =>
        consParts(normalize(t)).fold(report.errorAndAbort(s"Unexpected end of ${display(TypeRepr.of[T])}"))(_._2)
      )

    /**
     * The last element `outcome` would find were the elements before it removed, aborting with why when there's none,
     * or when an element after it might match once its types are known or is in a tail that isn't statically known
     */
    def lastFoundOrAbort: Found = {
      def refuse(why: String): Nothing =
        report.errorAndAbort(s"Cannot find the last ${display(TypeRepr.of[F])} in ${display(TypeRepr.of[T])}: $why")

      @annotation.tailrec def loop(f: Found, seen: List[TypeRepr]): Found = {
        val rest = normalize(f.tail)
        val direct = isDirectCons(rest)
        if (!direct && expandedBefore(rest, seen)) refuse(s"its tail `${display(rest)}` refers to itself")
        go(f.tail, f.head :: f.revInit, f.index + 1, Mode.Update, None, f.bounded, expanded = Nil) match {
          case g: Found => loop(g, if (direct) seen else rest :: seen)
          case Absent => f
          case Unknown(tail) => refuse(s"its tail `${display(tail)}` is not statically known")
          case Undecided(head, idx) => refuse(s"element `${display(head)}` at index $idx might match")
        }
      }
      loop(foundOrAbort, Nil)
    }

    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consSym.typeRef.appliedTo(List(h, t))

    /** The record with `newHead` in place of the field `found` */
    def replaced(found: Found, newHead: TypeRepr): TypeRepr =
      found.revInit.foldLeft(cons(newHead, found.tail))((acc, h) => cons(h, acc))

    /** The record without the field `found` */
    def removed(found: Found): TypeRepr =
      found.revInit.foldLeft(found.tail)((acc, h) => cons(h, acc))

    /** `t` with `f` appended, when `t` is spelled out as `H1 :: ... :: HNil` */
    def appended(t: TypeRepr, f: TypeRepr): Option[TypeRepr] = {
      @annotation.tailrec def loop(rest: TypeRepr, revInit: List[TypeRepr]): Option[TypeRepr] =
        rest.dealias match {
          case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol == consSym => loop(tail, head :: revInit)
          case end if end =:= hnilTpe => Some(revInit.foldLeft(consSym.typeRef.appliedTo(List(f, end)))((acc, h) => consSym.typeRef.appliedTo(List(h, acc))))
          case _ => None
        }
      loop(t, Nil)
    }

    /** Like `outcome`, but `Undecided` at the first element that doesn't match yet might once its types are known */
    def absence: Outcome = start(Mode.Prove)

    /**
     * In one walk, what `outcome` finds, or else what `absence` proves: `Found` for the first match, otherwise `Absent`
     * only when no element might match. A union of records whose arms end at different points falls back to `absence`.
     */
    def replaceOrAppend: Outcome =
      start(Mode.Update) match {
        case _: Unknown =>
          absence match {
            // Found in only some arms of a union of records, so neither replaced nor absent
            case Found(_, head, _, idx) => Undecided(head, idx)
            case other => other
          }
        case other => other
      }
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

    val f = lookup.foundOrAbort
    found(f.revInit, f.head, f.tail, f.index)
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

/**
 * The result of a record operation `Op` on the field `F` of `T`, with the index it acts on. Unlike `FindField`, it carries
 * only the result type its operation needs, so the macro builds only that one.
 */
private[formless] sealed trait FieldOp[T <: HList, F, Op] {
  type Out
  val index: Int
}

object FieldOp {
  /** Selects the first field `<:` `F`; `Out` is its value */
  sealed trait Select
  /** Removes the first field `<:` `F`; `Out` is its value and the rest of the record */
  sealed trait Remove
  /** Replaces the first field `=:=` `F` with `K ->> B`; `Out` is the new record */
  sealed trait Modify[K, B]
  /** Replaces the first field `=:=` `F` with `F`, or appends `F` (`index` -1) when no field could be; `Out` is the new record */
  sealed trait Update

  final class Inst[T <: HList, F, Op, O](val index: Int) extends FieldOp[T, F, Op] {
    final type Out = O
  }

  private def fieldOpImpl[T <: HList: Type, F: Type, Op: Type](using Quotes): Expr[FieldOp[T, F, Op]] =
    Type.of[Op] match {
      case '[Select] => selectImpl[T, F, Op]
      case '[Remove] => removeImpl[T, F, Op]
      case '[Modify[k, b]] => modifyImpl[T, F, Op, k, b]
      case '[Update] => updateImpl[T, F, Op]
      case _ => quotes.reflect.report.errorAndAbort(s"Unexpected record operation ${Type.show[Op]}")
    }

  private def inst[T <: HList: Type, F: Type, Op: Type, O: Type](idx: Int)(using Quotes): Expr[FieldOp[T, F, Op]] =
    '{ new Inst[T, F, Op, O](${ Expr(idx) }) }

  private def selectImpl[T <: HList: Type, F: Type, Op: Type](using Quotes): Expr[FieldOp[T, F, Op]] = {
    val lookup = FindField.Lookup[T, F, <:<]
    val f = lookup.foundOrAbort
    (lookup.keyValue(f.head)._2.asType: @unchecked) match {
      case '[v] => inst[T, F, Op, v](f.index)
    }
  }

  private def removeImpl[T <: HList: Type, F: Type, Op: Type](using Quotes): Expr[FieldOp[T, F, Op]] = {
    val lookup = FindField.Lookup[T, F, <:<]
    val f = lookup.foundOrAbort
    ((lookup.keyValue(f.head)._2.asType, lookup.removed(f).asType): @unchecked) match {
      case ('[v], '[rem]) => inst[T, F, Op, (v, rem)](f.index)
    }
  }

  private def modifyImpl[T <: HList: Type, F: Type, Op: Type, K: Type, B: Type](using Quotes): Expr[FieldOp[T, F, Op]] = {
    val lookup = FindField.Lookup[T, F, =:=]
    import lookup.q.reflect.*
    val f = lookup.foundOrAbort
    // As with `FindField`, the field must have a key and value that can be read
    val _ = lookup.keyValue(f.head)
    (lookup.replaced(f, TypeRepr.of[K ->> B]).asType: @unchecked) match {
      case '[o] => inst[T, F, Op, o](f.index)
    }
  }

  private def updateImpl[T <: HList: Type, F: Type, Op: Type](using Quotes): Expr[FieldOp[T, F, Op]] = {
    val lookup = FindField.Lookup[T, F, =:=]
    import lookup.q.reflect.*

    def instOf(out: TypeRepr, idx: Int): Expr[FieldOp[T, F, Op]] =
      (out.asType: @unchecked) match {
        case '[o] => inst[T, F, Op, o](idx)
      }
    def refuse(why: String): Nothing =
      report.errorAndAbort(s"Cannot update ${lookup.display(TypeRepr.of[T])} with ${lookup.display(TypeRepr.of[F])}: $why")

    lookup.replaceOrAppend match {
      case f @ lookup.Found(_, head, _, idx) =>
        // As with `FindField`, a field matched up to `=:=` is only replaced when its key and value can be read
        if (lookup.keyValueOpt(head).isEmpty) refuse(s"element `${lookup.display(head)}` at index $idx matches but isn't a field")
        instOf(lookup.replaced(f, TypeRepr.of[F]), idx)
      case lookup.Absent =>
        // Built directly for a record spelled out as `H1 :: ... :: HNil`, rather than left to `HList.Append` to reduce
        instOf(lookup.appended(TypeRepr.of[T], TypeRepr.of[F]).getOrElse(TypeRepr.of[HList.Append[T, F]]), -1)
      case lookup.Unknown(rest) =>
        refuse(s"its tail `${lookup.display(rest)}` is not statically known")
      case lookup.Undecided(head, idx) =>
        refuse(s"element `${lookup.display(head)}` at index $idx might match")
    }
  }

  transparent inline given fieldOpInst[T <: HList, F, Op]: FieldOp[T, F, Op] = ${ fieldOpImpl[T, F, Op] }
}

