package formless
package record

import formless.hlist.*
import scala.quoted.*

/** Walks a record or HList type for the record and HList operations, and keeps their macros from crashing */
private[formless] object Lookup {
  private class ConcreteLabelled[K, V]

  /** The most elements one lookup walks, across every walk it makes */
  private[formless] final val MaxSteps = 10000

  /** The most tails one lookup reduces from an alias or match type, across every walk it makes */
  private[formless] final val MaxExpansions = 200

  /**
   * Why a lookup was refused. Lookups return it as a value; each macro's entry point, through `guarded`, is the only place
   * that turns it into a compile error.
   */
  private[formless] enum LookupError {
    case NotFound(field: String, record: String)
    case Present(record: String, element: String, index: Int, field: String)
    case Refused(action: String, reason: Reason)
    case AbstractOperation(operation: String)
    case PlainList(list: String)

    def message: String = this match {
      case NotFound(field, record) => s"Failed to find field $field in record $record"
      case Present(record, element, index, field) => s"Record $record has field $element at index $index matching $field"
      case Refused(action, reason) => s"Cannot $action: ${reason.message}"
      case AbstractOperation(operation) => s"Cannot expand a lookup for the abstract operation $operation"
      case PlainList(list) => s"$list has no record fields"
    }
  }

  /** Why a `LookupError.Refused` lookup couldn't be done */
  private[formless] enum Reason {
    case TailUnknown(tail: String)
    case TailRefersToItself(tail: String)
    case MightMatch(element: String, index: Int)
    case NotAField(element: String, index: Int)
    case TooLong(max: Int)
    case TooManyReductions(max: Int)
    case NotSpelledOut(part: String)
    case ArmDiffers(element: String, index: Int, expected: String)
    case TooDeep
    case Failed(exception: String)

    def message: String = this match {
      case TailUnknown(tail) => s"its tail `$tail` is not statically known"
      case TailRefersToItself(tail) => s"its tail `$tail` refers to itself"
      case MightMatch(element, index) => s"element `$element` at index $index might match"
      case NotAField(element, index) => s"element `$element` at index $index matches but isn't a field"
      case TooLong(max) => s"it has more than $max elements, or never ends"
      case TooManyReductions(max) => s"more than $max of its tails are aliases or match types to reduce, so it may never end"
      case NotSpelledOut(part) => s"`$part` isn't spelled out as `H :: T`, so reinsert couldn't give one back"
      case ArmDiffers(element, index, expected) => s"one of its arms has `$element` at index $index, not `$expected`"
      case TooDeep => "the types are too deeply nested to look through"
      case Failed(exception) => s"looking through the types failed with $exception"
    }
  }

  /**
   * A macro's entry point: runs its body and reports a refused lookup as a compile error. An exception thrown while
   * walking the types is refused too, so a lookup never crashes the compiler, except for the compiler's own control flow
   * (aborting or suspending the expansion), which passes through, and fatal errors other than a stack overflow, which
   * aren't the lookup's to handle.
   */
  private[formless] def guarded[A](what: String)(body: => Either[LookupError, A])(using q: Quotes): A =
    scala.util.control.Exception.allCatch.either(body) match {
      case Right(result) => result.fold(e => q.reflect.report.errorAndAbort(e.message), identity)
      case Left(e) if isCompilerControl(e) => throw e
      case Left(_: StackOverflowError) => q.reflect.report.errorAndAbort(LookupError.Refused(what, Reason.TooDeep).message)
      case Left(scala.util.control.NonFatal(e)) =>
        q.reflect.report.errorAndAbort(LookupError.Refused(what, Reason.Failed(e.getClass.getName)).message)
      case Left(fatal) => throw fatal
    }

  // The compiler aborts an expansion with `StopMacroExpansion`, and suspends one that needs a definition it hasn't
  // compiled yet with an exception that isn't part of the public API. That one is looked up by class from the compiler's
  // own class loader, only once something has been thrown; a compiler without it has no suspensions to let through.
  private def isCompilerControl(e: Throwable)(using q: Quotes): Boolean =
    e.isInstanceOf[scala.quoted.runtime.StopMacroExpansion] ||
      scala.util.control.Exception.catching(classOf[ClassNotFoundException])
        .opt(Class.forName("dotty.tools.dotc.CompilationUnit$SuspendException", false, q.getClass.getClassLoader))
        .exists(_.isInstance(e))

  /** A literal singleton key, tagged with its type so that e.g. `1` and `1L` are different keys */
  private enum LiteralKey {
    case Str(value: String)
    case IntKey(value: Int)
    case LongKey(value: Long)
    case CharKey(value: Char)
    case BooleanKey(value: Boolean)
    case ShortKey(value: Short)
    case ByteKey(value: Byte)
    case FloatKey(value: Float)
    case DoubleKey(value: Double)
  }

  /**
   * Walks a record type looking for the first field `head` such that `Cmp[head, F]` holds.
   *
   * A record whose tail is not statically known (an abstract `L <: HList`) is `Unknown`: it neither contains nor lacks
   * the field, so neither `FieldOp` nor `FieldAbsent` resolves for it.
   */
  private[formless] final class Walk[T <: HList: Type, F: Type, Cmp[_, _]: Type](using val q: Quotes) {
    import q.reflect.*

    private enum Mode {
      case Find, Prove
      /** `pending` is the first element walked past that might have matched */
      case Update(pending: Option[Undecided])
    }

    sealed trait Outcome
    /**
     * `rest` is the record from the match on, as spelled in `T`, e.g. `tail.type` rather than its elements. `bounded`
     * when `head` was read off an abstract record's upper bound, so the element may be narrower.
     */
    final case class Found(revInit: List[TypeRepr], head: TypeRepr, tail: TypeRepr)(val rest: TypeRepr, val bounded: Boolean)
      extends Outcome {
      def index: Int = revInit.length
    }
    case object Absent extends Outcome
    final case class Unknown(rest: TypeRepr) extends Outcome
    /** `head` is the first element that might match once its types are known, after the elements `revInit` */
    final case class Undecided(revInit: List[TypeRepr], head: TypeRepr) extends Outcome {
      def index: Int = revInit.length
    }

    private val labelled = TypeRepr.of[->>]
    private val labelledSym = labelled.typeSymbol
    private val concreteLabelled = TypeRepr.of[ConcreteLabelled]
    private val concreteLabelledSym = concreteLabelled.typeSymbol
    private val consSym: Symbol = TypeRepr.of[Any :: HNil].typeSymbol
    private val hnilTpe = TypeRepr.of[HNil]
    private val fTpe = TypeRepr.of[F]
    private val cmpTpe = TypeRepr.of[Cmp]

    // A type parameter or abstract type member, which stands for some subtype of its bound; an opaque type is abstract
    // only outside its scope, where its bound is all that's known of it, so it's handled on its own
    private def isAbstract(sym: Symbol): Boolean = sym.isTypeParam || (sym.isAbstractType && !sym.flags.is(Flags.Opaque))

    /** `t`'s head and tail when `t` is spelled `H :: T` */
    private object DirectCons {
      def unapply(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
        t match {
          case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol == consSym => Some((head, tail))
          case _ => None
        }
    }

    private def labelledArgs(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t.dealias match {
        case AppliedType(tycon, List(k, v)) if tycon.typeSymbol == labelledSym => Some((k, v))
        case _ => None
      }

    // `show` throws CyclicReference for a type owned by a method whose result type is still being inferred
    def display(t: TypeRepr): String =
      scala.util.Try(t.show).orElse(scala.util.Try(t.typeSymbol.name)).getOrElse("<type>")

    // A type parameter or abstract type member stands for some subtype of its upper bound. A member's bound is taken as
    // seen from its prefix, so `c.H` for `c: C[Int]` and `trait C[A] { type H <: "a" ->> A }` is bounded by `"a" ->> Int`
    private def upperBound(t: TypeRepr): Option[TypeRepr] =
      t.dealias match {
        case tr: TypeRef if isAbstract(tr.typeSymbol) =>
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
    /** The key and value of the field `f`, or why it isn't one */
    def fieldKeyValue(f: Found): Either[LookupError, (TypeRepr, TypeRepr)] =
      keyValueOpt(f.head).toRight(LookupError.Refused(s"use $field in $record", Reason.NotAField(display(f.head), f.index)))

    /** `T` and `F` as written in messages */
    def record: String = display(TypeRepr.of[T])
    def field: String = display(fTpe)

    private def keyValueOpt(head: TypeRepr): Option[(TypeRepr, TypeRepr)] =
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

    // The match infers a key and value even for a type that isn't a subtype of any one field, e.g. `Nothing` as the
    // value of `(K ->> String) & ("b" ->> String)`, so only a key and value the head is proven a field of are kept
    private def matchedKeyValue(head: TypeRepr): Option[(TypeRepr, TypeRepr)] = {
      val concreteHead = deepDealias(head).substituteTypes(List(labelledSym), List(concreteLabelled))
      def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
      concreteHead.asType match {
        case '[ConcreteLabelled[k, v]] if concreteHead <:< TypeRepr.of[ConcreteLabelled[k, v]] =>
          Some((unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v])))
        case _ => None
      }
    }

    private def union(a: TypeRepr, b: TypeRepr): TypeRepr = if (a =:= b) a else OrType(a, b)

    private def conjuncts(t: TypeRepr): List[TypeRepr] =
      t.dealias match {
        case AndType(a, b) => conjuncts(a) ++ conjuncts(b)
        case other => List(other)
      }

    // The conjuncts keyed exactly like `F`, abstract keys included, e.g. `K ->> String` in `(K ->> String) & ("b" ->> String)`
    // looked up by `K`. Each is a supertype of the field, so the value is the intersection of theirs, or a supertype of
    // it when a conjunct whose key only might equal `F`'s is left out.
    private def intersectionKeyValue(t: AndType): Option[(TypeRepr, TypeRepr)] =
      fLabelledKey.flatMap { fk =>
        conjuncts(t)
          .flatMap(labelledOrBound)
          .filter(_._1 =:= fk)
          .reduceOption((x, y) => (x._1, if (x._2 =:= y._2) x._2 else AndType(x._2, y._2)))
      }

    // A field's literal key, e.g. `"a"`, `1`, `1L` or `'1'`
    private def literalKey(t: TypeRepr): Option[LiteralKey] =
      labelledArgs(t).flatMap { case (k, _) =>
        k.dealias match {
          case ConstantType(StringConstant(v)) => Some(LiteralKey.Str(v))
          case ConstantType(IntConstant(v)) => Some(LiteralKey.IntKey(v))
          case ConstantType(LongConstant(v)) => Some(LiteralKey.LongKey(v))
          case ConstantType(CharConstant(v)) => Some(LiteralKey.CharKey(v))
          case ConstantType(BooleanConstant(v)) => Some(LiteralKey.BooleanKey(v))
          case ConstantType(ShortConstant(v)) => Some(LiteralKey.ShortKey(v))
          case ConstantType(ByteConstant(v)) => Some(LiteralKey.ByteKey(v))
          case ConstantType(FloatConstant(v)) => Some(LiteralKey.FloatKey(v))
          case ConstantType(DoubleConstant(v)) => Some(LiteralKey.DoubleKey(v))
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
          !isAbstract(sym) && ground(tr.qualifier) &&
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
        // `x: String`, could become the same path as another one once the method is applied, and so could a member a
        // subclass can override, e.g. `val y: String` in a trait, which an implementation may define as `val y: x.type`.
        case tr: TermRef =>
          !tr.termSymbol.flags.is(Flags.Param) && !overridable(tr.termSymbol) && ground(tr.qualifier) && ground(tr.widen)
        case _: ConstantType | _: ThisType | _: NoPrefix => true
        case _ => false
      }
    }

    // A member of a class or trait that isn't final, nor in a final class or an object
    private def overridable(sym: Symbol): Boolean = {
      val owner = sym.owner
      owner.isClassDef && !sym.flags.is(Flags.Final) && !owner.flags.is(Flags.Final) && !owner.flags.is(Flags.Module)
    }

    // A bound that can't be read, e.g. one still being inferred, might be abstract, so it isn't ground
    private def opaqueBoundGround(tr: TypeRef, opaques: Set[Symbol]): Boolean =
      scala.util.Try(tr.qualifier.memberType(tr.typeSymbol)).toOption.exists {
        case TypeBounds(_, hi) => isGround(hi, opaques)
        case other => isGround(other, opaques)
      }

    // The key and value of `t` when it is a field, or is abstract and bounded by one
    private def labelledOrBound(t: TypeRepr): Option[(TypeRepr, TypeRepr)] = labelledArgs(t).orElse(upperBound(t).flatMap(labelledArgs))

    private def labelledKey(t: TypeRepr): Option[TypeRepr] = labelledOrBound(t).map(_._1)

    // An abstract element bounded by `"a" ->> V` may still be `("a" ->> V) & ("b" ->> W)`, a subtype of `"b" ->> Any`, so
    // its bound's key rules out other keys only under `=:=`. Bounding `F` instead is fine: `head <: F <: ("b" ->> W)`
    // and `head =:= F` both need `head`'s key to be `"b"`. Bounding both isn't: `H <: "a" ->> V` and `F <: "b" ->> W`
    // can both be `("a" ->> V) & ("b" ->> W)`.
    private val cmpIsEq = cmpTpe.typeSymbol == TypeRepr.of[=:=].typeSymbol

    private lazy val fLabelledKey = labelledKey(fTpe)

    // Whether `Cmp[head, F]` could hold once the abstract types in `head` or `F` are known. A `bounded` head was read
    // off the upper bound of an abstract record, so the element it stands for may be any subtype of it.
    private def mightMatch(head: TypeRepr, bounded: Boolean): Boolean =
      head.dealias match {
        // A union, e.g. the heads of `(A :: HNil) | (B :: HNil)` or a field typed `A | B`, may be any of its arms, so it
        // might match if any of them does
        case OrType(a, b) => List(a, b).exists(arm => cmpHolds(arm) || mightMatch(arm, bounded))
        case _ => mightMatchOne(head, bounded)
      }

    private def mightMatchOne(head: TypeRepr, bounded: Boolean): Boolean = {
      val headKey =
        if (cmpIsEq) labelledKey(head)
        else if (bounded) None
        else labelledArgs(head).map(_._1)
      // A key is exact when its type is itself a field, rather than bounded by one or read off an abstract record's bound
      def exactKeys = (labelledArgs(head).nonEmpty && !bounded) || labelledArgs(fTpe).nonEmpty
      (headKey, fLabelledKey) match {
        case (Some(hk), Some(fk)) if isGround(hk) && isGround(fk) && !(hk =:= fk) && (!cmpIsEq || exactKeys) => false
        // `head =:= F` needs `F` within `head`'s upper bound, e.g. never for `H <: String` and `Int`
        case _ if cmpIsEq && upperBound(head).exists(hi => isGround(hi) && isGround(fTpe) && !(fTpe <:< hi)) => false
        case _ => bounded || !(isGround(head) && isGround(fTpe))
      }
    }

    // `->>` is invariant in its key, so a field keyed by a different literal can never satisfy `Cmp[field, F]`. Keys are
    // compared as `LiteralKey`s, so literals of different types differ, as `1` and `1L` or `49` and `'1'` do.
    private val fKey = literalKey(fTpe)

    private def keyMismatch(head: TypeRepr): Boolean =
      fKey.exists(fk => literalKey(head).exists(_ != fk))

    private def cmpHolds(head: TypeRepr): Boolean =
      !keyMismatch(head) &&
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          // More than one witness, e.g. from local givens, still means `Cmp[head, F]` holds
          case _: AmbiguousImplicits => true
          // `ImplicitSearchResult` is an abstract type in the reflection API, so the failures can't be matched exhaustively
          case _ => false
        })

    // The head and tail of a record type: directly for `H :: T`, through the base type otherwise (an abstract record's
    // upper bound, an intersection, a singleton), and arm by arm for a union of records
    private def consParts(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t match {
        case DirectCons(head, tail) => Some((head, tail))
        case OrType(a, b) =>
          // Comparing entire tails here can recurse through their common prefix at every position. Carry the arms
          // forward so the walk only needs the next element from each.
          for {
            (ha, ta) <- consParts(a.dealias.simplified)
            (hb, tb) <- consParts(b.dealias.simplified)
          } yield (union(ha, hb), OrType(ta, tb))
        case _ =>
          t.baseType(consSym) match {
            case AppliedType(_, List(head, tail)) => Some((head, tail))
            case _ => None
          }
      }

    private inline def undecidedInUnion(t: TypeRepr, undecided: => Boolean): Boolean =
      t match {
        // Equal tails, including differently spelled aliases, behave like a single list. Check equality only
        // when an undecided head would otherwise stop the walk.
        case OrType(a, b) => undecided && !(a =:= b)
        case _ => false
      }

    private def isDirectCons(t: TypeRepr): Boolean =
      t match {
        case DirectCons(_, _) => true
        case OrType(a, b) => isDirectCons(a.dealias.simplified) && isDirectCons(b.dealias.simplified)
        case _ => false
      }

    // Every walk of this lookup counts against one budget, so a record type that never ends, e.g. a match type that
    // keeps producing elements as it reduces, is refused instead of walked forever. The counters are mutable so the
    // tail-recursive walk, which runs once per element of every lookup, needn't thread and return a budget.
    private var steps = 0
    private var expansions = 0

    /**
     * @param mode `Find` looks for the first match, stopping `Undecided` at an element of a union of lists that matches in
     *             only some arms, or might once its types are known, as every mode does; `Prove` also stops `Undecided`
     *             at any other element that might match once its types are known, and splits a union of records into
     *             arms that end at different points;
     *             `Update` looks for the first match like `Find`, but ends `Undecided` rather than `Absent` when an
     *             element it walked past might have matched
     * @param bounded whether the elements so far were read off an abstract record's upper bound, so each stands for
     *                some subtype of itself
     * @param expanded the abstract records already walked through their bound, so `type L <: Int :: L` doesn't loop
     */
    @annotation.tailrec
    private def go(
      rest: TypeRepr,
      revInit: List[TypeRepr],
      mode: Mode,
      bounded: Boolean,
      expanded: List[TypeRepr],
    ): Either[LookupError, Outcome] = {
      steps += 1
      // A tail that had to be reduced, e.g. an alias or a match type, may grow as it is reduced again, so these have a
      // smaller budget, stopping before the compiler's own recursion limit. A union of `H :: T`s doesn't grow as it's
      // walked, so only other tails count.
      if (!isDirectCons(rest)) expansions += 1
      // Most tails are already `H :: T`, which needs no normalizing
      val t = normalize(rest)
      val direct = isDirectCons(t)
      if (steps > Lookup.MaxSteps) Left(LookupError.Refused(s"look through $record", Reason.TooLong(Lookup.MaxSteps)))
      else if (expansions > Lookup.MaxExpansions)
        Left(LookupError.Refused(s"look through $record", Reason.TooManyReductions(Lookup.MaxExpansions)))
      else if (!direct && expandedBefore(t, expanded)) Right(Unknown(t))
      else consParts(t) match {
        case Some((head, tail)) =>
          // An opaque record's elements are read off its bound, e.g. `Any` for `opaque type O <: Any :: HNil = Int :: HNil`
          val nowBounded = bounded || !(direct || isGround(t)) || t.typeSymbol.flags.is(Flags.Opaque)
          lazy val undecided = mightMatch(head, nowBounded)
          if (cmpHolds(head)) Right(Found(revInit, head, tail)(rest, nowBounded))
          // An element of a union of lists that matches in only some arms, e.g. `String | Int` in
          // `(String :: Int :: HNil) | (Int :: Int :: HNil)` looked up by `Int`, is the first match in those arms, so no
          // later element is the first match in all of them
          else if ((mode == Mode.Prove && undecided) || undecidedInUnion(t, undecided)) Right(Undecided(revInit, head))
          else {
            val nowMode = mode match {
              case Mode.Update(None) if undecided => Mode.Update(Some(Undecided(revInit, head)))
              case same @ (Mode.Find | Mode.Prove | Mode.Update(_)) => same
            }
            go(tail, head :: revInit, nowMode, nowBounded, if (direct) expanded else t :: expanded)
          }
        case None =>
          if (t <:< hnilTpe) Right(mode match {
            case Mode.Update(Some(pending)) => pending
            case Mode.Find | Mode.Prove | Mode.Update(None) => Absent
          })
          else t match {
            // A union of records that don't all have another element, e.g. `HNil | (A :: HNil)`: absent only when it's
            // absent from every arm
            case OrType(a, b) if mode == Mode.Prove => absenceInArms(a, b, revInit, bounded, expanded)
            case _ => Right(Unknown(t))
          }
      }
    }

    private def absenceInArms(
      a: TypeRepr,
      b: TypeRepr,
      revInit: List[TypeRepr],
      bounded: Boolean,
      expanded: List[TypeRepr],
    ): Either[LookupError, Outcome] =
      go(a, revInit, Mode.Prove, bounded, expanded).flatMap {
        case Absent => go(b, revInit, Mode.Prove, bounded, expanded)
        case decided @ (_: Found | _: Unknown | _: Undecided) => Right(decided)
      }

    private def start(mode: Mode): Either[LookupError, Outcome] =
      go(TypeRepr.of[T], Nil, mode, bounded = false, expanded = Nil)

    /** The first field `Cmp[head, F]` holds for, or why there's none */
    def found: Either[LookupError, Found] = {
      def refuse(reason: Reason): Left[LookupError, Nothing] = Left(LookupError.Refused(s"find field $field in record $record", reason))
      start(Mode.Find).flatMap {
        case f: Found => Right(f)
        case Absent => Left(LookupError.NotFound(field, record))
        case Unknown(rest) => refuse(Reason.TailUnknown(display(rest)))
        case u: Undecided => refuse(Reason.MightMatch(display(u.head), u.index))
      }
    }

    // The abstract type a record type stands for, e.g. `L` for `c.L[Int]`, to stop a walk through bounds that refer to
    // themselves even as their type arguments change, e.g. `type L[A] <: Int :: L[List[A]]`
    private def abstractSymbol(t: TypeRepr): Option[Symbol] = {
      val sym = t match {
        case AppliedType(tycon, _) => tycon.typeSymbol
        case _ => t.typeSymbol
      }
      Option.when(!sym.isNoSymbol && isAbstract(sym))(sym)
    }

    private def expandedBefore(t: TypeRepr, expanded: List[TypeRepr]): Boolean = {
      val sym = abstractSymbol(t)
      expanded.exists(e => e =:= t || (sym.nonEmpty && abstractSymbol(e) == sym))
    }

    private def normalize(t: TypeRepr): TypeRepr =
      t match {
        case DirectCons(_, _) => t
        case _ => t.dealias.simplified
      }

    /**
     * The last element `found` would find were the elements before it removed, or why there's none: no match, or an
     * element after it might match once its types are known, or is in a tail that isn't statically known
     */
    def lastFound: Either[LookupError, Found] = {
      def refuse(reason: Reason): Left[LookupError, Nothing] = Left(LookupError.Refused(s"find the last $field in $record", reason))

      @annotation.tailrec def loop(f: Found, seen: List[TypeRepr]): Either[LookupError, Found] = {
        val rest = normalize(f.tail)
        val direct = isDirectCons(rest)
        if (!direct && expandedBefore(rest, seen)) refuse(Reason.TailRefersToItself(display(rest)))
        else go(f.tail, f.head :: f.revInit, Mode.Update(None), f.bounded, expanded = Nil) match {
          case Right(g: Found) => loop(g, if (direct) seen else rest :: seen)
          case Right(Absent) => Right(f)
          case Right(Unknown(tail)) => refuse(Reason.TailUnknown(display(tail)))
          case Right(u: Undecided) => refuse(Reason.MightMatch(display(u.head), u.index))
          case Left(e) => Left(e)
        }
      }
      found.flatMap(loop(_, Nil))
    }

    def cons(h: TypeRepr, t: TypeRepr): TypeRepr = consSym.typeRef.appliedTo(List(h, t))

    /** The record with `newHead` in place of the field `found` */
    def replaced(found: Found, newHead: TypeRepr): TypeRepr =
      found.revInit.foldLeft(cons(newHead, found.tail))((acc, h) => cons(h, acc))

    /** The record without the field `found` */
    def removed(found: Found): TypeRepr =
      found.revInit.foldLeft(found.tail)((acc, h) => cons(h, acc))

    /**
     * `t` with `f` appended, when `t` is spelled out as `H1 :: ... :: HNil`, or is a union of such records, which each
     * get `f` appended
     */
    def appended(t: TypeRepr, f: TypeRepr): Option[TypeRepr] =
      t.dealias match {
        case DirectCons(head, tail) => appended(tail, f).map(cons(head, _))
        case OrType(a, b) =>
          for {
            x <- appended(a, f)
            y <- appended(b, f)
          } yield union(x, y)
        case end if end =:= hnilTpe => Some(cons(f, end))
        case _ => None
      }

    /**
     * `T` without the element `found`, for `Remove`, taken from each arm of a union of lists separately so the arms'
     * other elements stay together, e.g. `(String :: String :: HNil) | (Boolean :: Boolean :: HNil)` from removing the
     * `Int` in `(String :: Int :: String :: HNil) | (Boolean :: Int :: Boolean :: HNil)`. Refused when `reinsert` couldn't
     * give back a `T`: it rebuilds the list up to `found`, so every part of `T` up to there must be spelled out as `H :: T`
     * (or be a union of such lists), not a singleton, intersection or abstract list that it would claim to rebuild, and
     * every arm of a union must have `F` there, so an element taken from one arm can't be put back into another.
     */
    def removedReinsertably(found: Found): Either[LookupError, TypeRepr] = {
      def refuse(reason: Reason): Left[LookupError, Nothing] = Left(LookupError.Refused(s"remove $field from $record", reason))
      // `before` is the elements of `found`'s prefix still to pass in this arm; the walk found an element after them in
      // every arm, so every arm spelled out as `H :: T` this far has one
      def loop(t: TypeRepr, before: List[TypeRepr]): Either[LookupError, TypeRepr] =
        normalize(t) match {
          case OrType(a, b) => loop(a, before).flatMap(x => loop(b, before).map(union(x, _)))
          case DirectCons(head, tail) =>
            before match {
              case scala.collection.immutable.::(_, more) => loop(tail, more).map(cons(head, _))
              case Nil => if (head =:= fTpe) Right(tail) else refuse(Reason.ArmDiffers(display(head), found.index, field))
            }
          case _ => refuse(Reason.NotSpelledOut(display(t)))
        }
      loop(TypeRepr.of[T], found.revInit)
    }

    /**
     * Whether `T` is spelled out as `H1 :: ... :: HNil`, or a union of such lists, with no element that is a field or,
     * once its types are known, might be one
     */
    def isPlainList: Boolean = {
      // `todo` holds the lists still to check, so each arm of a union, e.g. `HNil | (Int :: HNil)`, is checked on its own
      @annotation.tailrec def loop(todo: List[TypeRepr], steps: Int): Boolean =
        todo match {
          case Nil => true
          case scala.collection.immutable.::(t, more) =>
            val n = normalize(t)
            if (steps > Lookup.MaxSteps) false
            else n match {
              case OrType(a, b) => loop(a :: b :: more, steps + 1)
              case DirectCons(head, tail) =>
                keyValueOpt(head).isEmpty && isGround(head) && loop(tail :: more, steps + 1)
              case _ => n <:< hnilTpe && isGround(n) && loop(more, steps + 1)
            }
        }
      loop(List(TypeRepr.of[T]), 0)
    }

    /** Like `found`, but `Undecided` at the first element that doesn't match yet might once its types are known */
    def absence: Either[LookupError, Outcome] = start(Mode.Prove)

    /**
     * In one walk, what `found` finds, or else what `absence` proves: `Found` for the first match, otherwise `Absent`
     * only when no element might match. A union of records whose arms end at different points falls back to `absence`.
     */
    def replaceOrAppend: Either[LookupError, Outcome] =
      start(Mode.Update(None)).flatMap {
        case _: Unknown =>
          absence.map {
            // Found in only some arms of a union of records, so neither replaced nor absent
            case f: Found => Undecided(f.revInit, f.head)
            case proven @ (Absent | _: Unknown | _: Undecided) => proven
          }
        case decided @ (_: Found | Absent | _: Undecided) => Right(decided)
      }
  }

}

/**
 * Evidence that no field `head` of the record `T` satisfies `Cmp[head, F]`, whatever its abstract types turn out to be.
 * Unlike the absence of a lookup that succeeds, it does not resolve for a record with an abstract tail, with an element
 * that might match once its type parameters are known, or with a field that satisfies `Cmp[head, F]` but that `FieldOp`
 * can't use.
 */
@scala.annotation.implicitNotFound("Cannot prove that no element of ${T} matches ${F}")
private[formless] opaque type FieldAbsent[T <: HList, F, Cmp[_, _]] = Unit

object FieldAbsent {
  // Inside the opaque type's scope, so the proof is made without a cast; the macro below is the only caller
  private[formless] def proof[T <: HList, F, Cmp[_, _]]: FieldAbsent[T, F, Cmp] = ()

  private def fieldAbsentImpl[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Expr[FieldAbsent[T, F, Cmp]] =
    Lookup.guarded("prove a field absent from a record")(fieldAbsentImpl0[T, F, Cmp])

  private def fieldAbsentImpl0[T <: HList: Type, F: Type, Cmp[_, _]: Type](using Quotes): Either[Lookup.LookupError, Expr[FieldAbsent[T, F, Cmp]]] = {
    val lookup = Lookup.Walk[T, F, Cmp]
    import Lookup.{LookupError, Reason}
    import lookup.{field, record}
    def refuse(reason: Reason) = Left(LookupError.Refused(s"prove $field is absent from $record", reason))

    lookup.absence.flatMap {
      case lookup.Absent => Right('{ FieldAbsent.proof[T, F, Cmp] })
      case f: lookup.Found => Left(LookupError.Present(record, lookup.display(f.head), f.index, field))
      case lookup.Unknown(rest) => refuse(Reason.TailUnknown(lookup.display(rest)))
      case u: lookup.Undecided => refuse(Reason.MightMatch(lookup.display(u.head), u.index))
    }
  }

  transparent inline given fieldAbsentInst[T <: HList, F, Cmp[_, _]]: FieldAbsent[T, F, Cmp] = ${ fieldAbsentImpl[T, F, Cmp] }
}

/**
 * The result of a record operation `Op` on the field `F` of `T`, with the index it acts on. It carries only the result type
 * its operation needs, so the macro builds only that one.
 */
@scala.annotation.implicitNotFound("Cannot find a field of ${T} matching ${F}")
private[formless] sealed trait FieldOp[T <: HList, F, Op <: FieldOp.Op] extends Serializable {
  type Out
  val index: Int
}

object FieldOp {
  sealed trait Op
  /** Selects the first field `<:` `F`; `Out` is its value */
  sealed trait Select extends Op
  /** Removes the first field `<:` `F`; `Out` is its value and the rest of the record */
  sealed trait Remove extends Op

  // Built only by the macro below, which proves `index` is `O`'s field of `T`
  private[formless] final class Inst[T <: HList, F, Op <: FieldOp.Op, O](val index: Int) extends FieldOp[T, F, Op] {
    final type Out = O
  }

  private def fieldOpImpl[T <: HList: Type, F: Type, Op <: FieldOp.Op: Type](using Quotes): Expr[FieldOp[T, F, Op]] =
    Lookup.guarded("look up a record field")(fieldOpImpl0[T, F, Op])

  // `Op`'s bound limits it to the operations below; the last case is an abstract `Op`, which can't be expanded
  private def fieldOpImpl0[T <: HList: Type, F: Type, Op <: FieldOp.Op: Type](using Quotes): Either[Lookup.LookupError, Expr[FieldOp[T, F, Op]]] = {
    val lookup = Lookup.Walk[T, F, <:<]
    def inst[O: Type](f: lookup.Found): Expr[FieldOp[T, F, Op]] = '{ new Inst[T, F, Op, O](${ Expr(f.index) }) }

    Type.of[Op] match {
      case '[Select] =>
        for {
          f <- lookup.found
          kv <- lookup.fieldKeyValue(f)
        } yield (kv._2.asType: @unchecked) match {
          case '[v] => inst[v](f)
        }
      case '[Remove] =>
        for {
          f <- lookup.found
          kv <- lookup.fieldKeyValue(f)
        } yield ((kv._2.asType, lookup.removed(f).asType): @unchecked) match {
          case ('[v], '[rem]) => inst[(v, rem)](f)
        }
      case _ => Left(Lookup.LookupError.AbstractOperation(Type.show[Op]))
    }
  }

  transparent inline given fieldOpInst[T <: HList, F, Op <: FieldOp.Op]: FieldOp[T, F, Op] = ${ fieldOpImpl[T, F, Op] }
}

/**
 * How `Updater` puts the field `F` into the record `T`: it replaces the first field `=:=` `F`, or appends `F` when no
 * field could be, deciding which in one walk of the record. `Out` is the new record.
 */
@scala.annotation.implicitNotFound("Cannot update ${T} with ${F}")
private[formless] sealed trait FieldUpdate[T <: HList, F] extends Serializable {
  type Out
  def apply(t: T, f: F): Out
}

object FieldUpdate {
  private[formless] final class Replace[T <: HList, F, O](index: Int) extends FieldUpdate[T, F] {
    final type Out = O
    def apply(t: T, f: F): O = HList.updatedAt(t, index, _ => f).asInstanceOf[O]
  }

  private[formless] final class Append[T <: HList, F, O] extends FieldUpdate[T, F] {
    final type Out = O
    def apply(t: T, f: F): O = HList.append(t, f).asInstanceOf[O]
  }

  private def fieldUpdateImpl[T <: HList: Type, F: Type](using Quotes): Expr[FieldUpdate[T, F]] =
    Lookup.guarded("update a record field")(fieldUpdateImpl0[T, F])

  private def fieldUpdateImpl0[T <: HList: Type, F: Type](using Quotes): Either[Lookup.LookupError, Expr[FieldUpdate[T, F]]] = {
    val lookup = Lookup.Walk[T, F, =:=]
    import lookup.q.reflect.*
    import Lookup.{LookupError, Reason}
    def refuse(reason: Reason) =
      Left(LookupError.Refused(s"update ${lookup.record} with ${lookup.field}", reason))

    lookup.replaceOrAppend.flatMap {
      case f: lookup.Found =>
        // A field matched up to `=:=` is only replaced when its key and value can be read
        lookup.fieldKeyValue(f).map(_ =>
          (lookup.replaced(f, TypeRepr.of[F]).asType: @unchecked) match {
            case '[o] => '{ new Replace[T, F, o](${ Expr(f.index) }) }
          }
        )
      case lookup.Absent =>
        // Built directly for a record spelled out as `H1 :: ... :: HNil`, rather than left to `HList.Append` to reduce
        Right((lookup.appended(TypeRepr.of[T], TypeRepr.of[F]).getOrElse(TypeRepr.of[HList.Append[T, F]]).asType: @unchecked) match {
          case '[o] => '{ new Append[T, F, o] }
        })
      case lookup.Unknown(rest) => refuse(Reason.TailUnknown(lookup.display(rest)))
      case u: lookup.Undecided => refuse(Reason.MightMatch(lookup.display(u.head), u.index))
    }
  }

  transparent inline given fieldUpdateInst[T <: HList, F]: FieldUpdate[T, F] = ${ fieldUpdateImpl[T, F] }
}

/**
 * The first element of `T` that is `=:=` to `F`, or for `Keyed` the first field `<:` `F`, with `T` as a type lambda
 * `Replaced` of what replaces it. Unlike
 * `FieldOp`, the replacement isn't one of its type arguments, so `Modifier` and `Replacer` can leave the new value's
 * type to be inferred, e.g. from the expected result type, after the element is found.
 */
@scala.annotation.implicitNotFound("Cannot find an element of ${T} matching ${F} to replace")
private[formless] sealed trait ReplaceAt[T <: HList, F, Kind <: ReplaceAt.Kind] extends Serializable {
  type Replaced[A] <: HList
  val index: Int
}

object ReplaceAt {
  sealed trait Kind
  /** `F` is a record field `K ->> A`, whose key and value must be readable */
  sealed trait Field extends Kind
  /** `F` is `K ->> Any`, standing for the first field keyed `K`, the one `Selector` reads */
  sealed trait Keyed extends Kind
  /** `F` is an HList element type */
  sealed trait Elem extends Kind

  private[formless] final class Inst[T <: HList, F, Kind <: ReplaceAt.Kind, R[_] <: HList](val index: Int) extends ReplaceAt[T, F, Kind] {
    final type Replaced[A] = R[A]
  }

  private def replaceAtImpl[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type](using Quotes): Expr[ReplaceAt[T, F, Kind]] =
    Lookup.guarded("find an element to replace")(replaceAtImpl0[T, F, Kind])

  // A `Keyed` field is found up to `<:<`, as `Selector` finds one; a `Field` or an element up to `=:=`. `Kind`'s bound
  // limits it to these; the last case is an abstract `Kind`, which can't be expanded.
  private def replaceAtImpl0[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type](using Quotes): Either[Lookup.LookupError, Expr[ReplaceAt[T, F, Kind]]] =
    Type.of[Kind] match {
      case '[Keyed] => replaceAtWith[T, F, Kind, <:<](readField = true)
      case '[Field] => replaceAtWith[T, F, Kind, =:=](readField = true)
      case '[Elem] => replaceAtWith[T, F, Kind, =:=](readField = false)
      case _ => Left(Lookup.LookupError.AbstractOperation(Type.show[Kind]))
    }

  /** @param readField whether the element must be a field whose key and value can be read */
  private def replaceAtWith[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type, Cmp[_, _]: Type](readField: Boolean)(
    using Quotes,
  ): Either[Lookup.LookupError, Expr[ReplaceAt[T, F, Kind]]] = {
    val walk = Lookup.Walk[T, F, Cmp]
    import walk.q.reflect.*

    for {
      f <- walk.found
      _ <- if (readField) walk.fieldKeyValue(f).map(_ => ()) else Right(())
    } yield {
      val replaced = TypeLambda(List("a"), _ => List(TypeBounds.empty), tl => walk.replaced(f, tl.param(0)))
      val instSym = TypeRepr.of[Inst[HNil, Any, Elem, [a] =>> HNil]].typeSymbol
      val targs = List(TypeRepr.of[T], TypeRepr.of[F], TypeRepr.of[Kind], replaced)
      New(Inferred(instSym.typeRef.appliedTo(targs)))
        .select(instSym.primaryConstructor)
        .appliedToTypes(targs)
        .appliedTo(Literal(IntConstant(f.index)))
        .asExprOf[ReplaceAt[T, F, Kind]]
    }
  }

  transparent inline given replaceAtInst[T <: HList, F, Kind <: ReplaceAt.Kind]: ReplaceAt[T, F, Kind] = ${ replaceAtImpl[T, F, Kind] }
}

/**
 * Evidence that `T` is, or might be, a record: it resolves for anything but a list spelled out as `H1 :: ... :: HNil`
 * with no element that is or might be a field. The record syntax's `apply`, `updateWith` and `replace`, whose names
 * `HList`s also have, require it, so using them on a plain `HList` explains why instead of reporting a missing field.
 */
@scala.annotation.implicitNotFound("${T} has no record fields, so this `apply`, `updateWith` or `replace` is the record syntax that importing formless.record adds to every HList, not the HList method of the same name. To call the HList method, write formless.hlist.HList.toHListOps(list).updateWith(...), or don't import the record syntax in this file.")
private[formless] sealed trait MaybeRecord[T <: HList]

object MaybeRecord {
  private[formless] final class Proof[T <: HList] extends MaybeRecord[T]

  private def maybeRecordImpl[T <: HList: Type](using Quotes): Expr[MaybeRecord[T]] =
    Lookup.guarded("check for record fields")(maybeRecordImpl0[T])

  private def maybeRecordImpl0[T <: HList: Type](using Quotes): Either[Lookup.LookupError, Expr[MaybeRecord[T]]] = {
    val lookup = Lookup.Walk[T, Nothing, <:<]
    if (lookup.isPlainList) Left(Lookup.LookupError.PlainList(lookup.display(lookup.q.reflect.TypeRepr.of[T])))
    else Right('{ new Proof[T] })
  }

  transparent inline given maybeRecordInst[T <: HList]: MaybeRecord[T] = ${ maybeRecordImpl[T] }
}

/** Never resolves: a record's `replace` takes a key and a value */
@scala.annotation.implicitNotFound("A record's replace takes a key and a value, e.g. record.replace(\"key\", value)")
private[formless] sealed trait ReplaceTakesAKey[T <: HList] {
  type Out
  def out: Out
}

