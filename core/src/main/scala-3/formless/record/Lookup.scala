package formless
package record

import formless.hlist.*
import scala.quoted.*
import scala.util.control.TailCalls.{TailRec, done, tailcall}

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
    case StrictFieldSubtype(element: String, index: Int)
    case DifferentFieldValue(key: String, existing: String, requested: String)
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
      case StrictFieldSubtype(element, index) => s"field `$element` at index $index has the same key and is a strict subtype of the requested field"
      case DifferentFieldValue(key, existing, requested) =>
        s"the record already has field $key with value type $existing, so updating it with $requested would add a second $key field. Use updateWith($key)(...) to change the field's type, or remove it first."
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

  enum Target {
    case Element, ExactField, UpdateField
  }

  /**
   * Walks a record type looking for the first field `head` such that `Cmp[head, F]` holds.
   *
   * A record whose tail is not statically known (an abstract `L <: HList`) is `Unknown`: it neither contains nor lacks
   * the field, so neither `FieldOp` nor `FieldAbsent` resolves for it.
   */
  // TailCalls keeps the continuations of nested type trees on the heap. Linear spines use compiler-checked tail recursion.
  private[formless] final class Walk[T <: HList: Type, F: Type, Cmp[_, _]: Type](target: Target)(using val q: Quotes) {
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

    private def isCons(t: TypeRepr): Boolean =
      t match {
        case a: AppliedType => a.tycon.typeSymbol == consSym
        case _ => false
      }

    /** `t`'s head and tail when `t` is spelled `H :: T` */
    private object DirectCons {
      def unapply(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
        t match {
          case a: AppliedType if isCons(a) =>
            a.args match {
              case List(head, tail) => Some((head, tail))
              case _ => None
            }
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
      labelledArgs(head).orElse(indirectKeyValue(head).result)

    private def keyValueStep(head: TypeRepr): TailRec[Option[(TypeRepr, TypeRepr)]] =
      labelledArgs(head) match {
        case some @ Some(_) => done(some)
        case None => indirectKeyValue(head)
      }

    private def indirectKeyValue(head: TypeRepr): TailRec[Option[(TypeRepr, TypeRepr)]] =
      upperBound(head) match {
        case Some(bound) => tailcall(keyValueStep(bound))
        case None =>
          head.dealias match {
            case OrType(a, b) =>
              tailcall(keyValueStep(a)).flatMap {
                case Some((ka, va)) =>
                  tailcall(keyValueStep(b)).map(_.map { case (kb, vb) => (union(ka, kb), union(va, vb)) })
                case None => done(None)
              }
            // A quoted match on an intersection would pick a key and value from neither conjunct in particular.
            case and: AndType => done(intersectionKeyValue(and).orElse(matchedKeyValue(head)))
            case _ => done(matchedKeyValue(head))
          }
      }

    // Aliases inside intersections and unions too, e.g. `A` in `A & Serializable` for `type A = "a" ->> Int`.
    private def deepDealias(t: TypeRepr): TailRec[TypeRepr] =
      t.dealias match {
        case AndType(a, b) =>
          tailcall(deepDealias(a)).flatMap(x => tailcall(deepDealias(b)).map(y => AndType(x, y)))
        case OrType(a, b) =>
          tailcall(deepDealias(a)).flatMap(x => tailcall(deepDealias(b)).map(y => OrType(x, y)))
        case AnnotatedType(u, _) => tailcall(deepDealias(u))
        case other => done(other)
      }

    // The match infers a key and value even for a type that isn't a subtype of any one field, e.g. `Nothing` as the
    // value of `(K ->> String) & ("b" ->> String)`, so only a key and value the head is proven a field of are kept
    private def matchedKeyValue(head: TypeRepr): Option[(TypeRepr, TypeRepr)] = {
      val concreteHead = deepDealias(head).result.substituteTypes(List(labelledSym), List(concreteLabelled))
      def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))
      concreteHead.asType match {
        case '[ConcreteLabelled[k, v]] if concreteHead <:< TypeRepr.of[ConcreteLabelled[k, v]] =>
          Some((unConcrete(TypeRepr.of[k]), unConcrete(TypeRepr.of[v])))
        case _ => None
      }
    }

    private def union(a: TypeRepr, b: TypeRepr): TypeRepr = if (a =:= b) a else OrType(a, b)

    private def conjuncts(t: TypeRepr): List[TypeRepr] = {
      @annotation.tailrec def loop(todo: List[TypeRepr], rev: List[TypeRepr]): List[TypeRepr] = todo match {
        case scala.collection.immutable.::(next, rest) => next.dealias match {
          case AndType(a, b) => loop(a :: b :: rest, rev)
          case other => loop(rest, other :: rev)
        }
        case Nil => rev.reverse
      }
      loop(t :: Nil, Nil)
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

    // A literal key, e.g. `"a"`, `1`, `1L` or `'1'`
    private def literalKey(k: TypeRepr): Option[LiteralKey] =
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

    // Free of type parameters and abstract type members, so a failed `Cmp` check against it is final
    private def isGround(t: TypeRepr): Boolean = {
      sealed trait Check
      final case class Inspect(tpe: TypeRepr, opaques: Set[Symbol]) extends Check
      final case class InspectOpaque(ref: TypeRef, opaques: Set[Symbol]) extends Check

      @annotation.tailrec def loop(t: TypeRepr, opaques: Set[Symbol], todo: List[Check]): Boolean = {
        t.dealias match {
          case tr: TypeRef =>
            val sym = tr.typeSymbol
            val opaque = sym.flags.is(Flags.Opaque)
            if (isAbstract(sym)) false
            else {
              val next = if (opaque && sym != labelledSym && !opaques(sym)) InspectOpaque(tr, opaques + sym) :: todo else todo
              loop(tr.qualifier, opaques, next)
            }
          case AppliedType(tycon, args) => loop(tycon, opaques, args.map(Inspect(_, opaques)) ::: todo)
          case AndType(a, b) => loop(a, opaques, Inspect(b, opaques) :: todo)
          case OrType(a, b) => loop(a, opaques, Inspect(b, opaques) :: todo)
          case AnnotatedType(u, _) => loop(u, opaques, todo)
          case Refinement(parent, _, info) => loop(parent, opaques, Inspect(info, opaques) :: todo)
          case TypeBounds(lo, hi) => loop(lo, opaques, Inspect(hi, opaques) :: todo)
          case ByNameType(u) => loop(u, opaques, todo)
          case mt: MethodType => loop(mt.resType, opaques, mt.paramTypes.map(Inspect(_, opaques)) ::: todo)
          // A lambda's own type parameters are bound within it.
          case tl: TypeLambda => loop(tl.resType, opaques, tl.paramBounds.map(Inspect(_, opaques)) ::: todo)
          case pt: PolyType => loop(pt.resType, opaques, pt.paramBounds.map(Inspect(_, opaques)) ::: todo)
          case RecursiveType(u) => loop(u, opaques, todo)
          // A singleton of an abstract or overridable term could end up a subtype of a type that it doesn't yet match.
          case tr: TermRef =>
            !tr.termSymbol.flags.is(Flags.Param) && !overridable(tr.termSymbol) &&
              loop(tr.qualifier, opaques, Inspect(tr.widen, opaques) :: todo)
          case _: ConstantType | _: ThisType | _: NoPrefix | _: ParamRef | _: RecursiveThis =>
            todo match {
              case Nil => true
              case scala.collection.immutable.::(Inspect(next, seen), rest) => loop(next, seen, rest)
              case scala.collection.immutable.::(InspectOpaque(ref, seen), rest) =>
                // A bound that can't be read might be abstract.
                scala.util.Try(ref.qualifier.memberType(ref.typeSymbol)).toOption match {
                  case Some(TypeBounds(_, hi)) => loop(hi, seen, rest)
                  case Some(other) => loop(other, seen, rest)
                  case None => false
                }
            }
          case _ => false
        }
      }

      loop(t, Set.empty, Nil)
    }

    // A member of a class or trait that isn't final, nor in a final class or an object
    private def overridable(sym: Symbol): Boolean = {
      val owner = sym.owner
      owner.isClassDef && !sym.flags.is(Flags.Final) && !owner.flags.is(Flags.Final) && !owner.flags.is(Flags.Module)
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
    private lazy val fIsGround = isGround(fTpe)

    // Whether `Cmp[head, F]` could hold once the abstract types in `head` or `F` are known. A `bounded` head was read
    // off the upper bound of an abstract record, so the element it stands for may be any subtype of it.
    private def mightMatch(head: TypeRepr, bounded: Boolean): Boolean = {
      @annotation.tailrec def loop(next: TypeRepr, pending: List[TypeRepr], arm: Boolean): Boolean =
        if (arm && !keyMismatch(next) && cmpHolds(next)) true
        else next.dealias match {
          case OrType(a, b) => loop(a, b :: pending, arm = true)
          case _ =>
            if (mightMatchOne(next, bounded)) true
            else pending match {
              case scala.collection.immutable.::(t, rest) => loop(t, rest, arm = true)
              case Nil => false
            }
        }
      loop(head, Nil, arm = false)
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
        case _ if cmpIsEq && upperBound(head).exists(hi => isGround(hi) && fIsGround && !(fTpe <:< hi)) => false
        case _ => bounded || !(isGround(head) && fIsGround)
      }
    }

    // `->>` is invariant in its key, so a field keyed by a different literal can never satisfy `Cmp[field, F]`. Keys are
    // compared as `LiteralKey`s, so literals of different types differ, as `1` and `1L` or `49` and `'1'` do.
    private val fKey = labelledArgs(fTpe).flatMap(x => literalKey(x._1))

    private def keyMismatch(head: TypeRepr): Boolean =
      fKey.exists(fk => labelledArgs(head).flatMap(x => literalKey(x._1)).exists(_ != fk))

    private lazy val cmpIsSubtype = cmpTpe.typeSymbol == TypeRepr.of[<:<].typeSymbol

    private def cmpHolds(head: TypeRepr): Boolean =
      // For literal fields, use the compiler's relation directly when it proves the match. Otherwise keep searching
      // for witnesses supplied by callers, including ambiguous witnesses when proving absence.
      (fKey.nonEmpty && ((cmpIsEq && (head =:= fTpe)) || (cmpIsSubtype && (head <:< fTpe)))) ||
        (Implicits.search(cmpTpe.appliedTo(List(head, fTpe))) match {
          case _: ImplicitSearchSuccess => true
          case _: AmbiguousImplicits => true
          // The reflection API's ImplicitSearchResult is abstract.
          case _ => false
        })

    private lazy val exactFieldKey = keyValueOpt(fTpe).map(_._1)

    // Selector can read a field with a narrower type. An exact field lookup must stop there, even if a later field
    // matches exactly or an updater would otherwise append. For a union, this can happen in just one arm.
    private def strictFieldSubtype(head: TypeRepr): Option[TypeRepr] = {
      @annotation.tailrec def loop(next: TypeRepr, pending: List[TypeRepr]): Option[TypeRepr] =
        if ((next <:< fTpe) && !(next =:= fTpe) && exactFieldKey.exists(fk => keyValueOpt(next).exists(_._1 =:= fk))) Some(next)
        else next.dealias match {
          case OrType(a, b) => loop(a, b :: pending)
          case _ => pending match {
            case scala.collection.immutable.::(t, rest) => loop(t, rest)
            case Nil => None
          }
        }
      loop(head, Nil)
    }

    private lazy val requestedField = keyValueOpt(fTpe)

    private def sameUpdateKey(hk: TypeRepr, fk: TypeRepr): Boolean =
      (literalKey(hk), literalKey(fk)) match {
        case (Some(h), Some(f)) => h == f
        case _ => isGround(hk) && isGround(fk) && (hk =:= fk)
      }

    // A collision in any union arm prevents appending, even if the other arms have different keys or lengths.
    private def updateConflict(head: TypeRepr, index: Int, bounded: Boolean): Option[Reason] =
      requestedField.flatMap { case (fk, fv) =>
        @annotation.tailrec def loop(todo: List[TypeRepr]): Option[Reason] = todo match {
          case scala.collection.immutable.::(next, rest) => next.dealias match {
            case OrType(a, b) => loop(a :: b :: rest)
            case _ =>
              keyValueOpt(next) match {
                case Some((hk, hv)) if sameUpdateKey(hk, fk) && !(next =:= fTpe) =>
                  if (!bounded && isGround(next) && fIsGround)
                    Some(Reason.DifferentFieldValue(display(fk), display(hv), display(fv)))
                  else Some(Reason.MightMatch(display(next), index))
                case _ => loop(rest)
              }
          }
          case Nil => None
        }
        loop(head :: Nil)
      }

    // The head and tail of a record type: directly for `H :: T`, through the base type otherwise (an abstract record's
    // upper bound, an intersection, a singleton), and arm by arm for a union of records.
    private def consParts(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t match {
        case DirectCons(head, tail) => Some((head, tail))
        case OrType(a, b) => unionCons(a, b).result
        case _ => baseCons(t)
      }

    private def baseCons(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      t.baseType(consSym) match {
        case AppliedType(_, List(head, tail)) => Some((head, tail))
        case _ => None
      }

    private def unionCons(a: TypeRepr, b: TypeRepr): TailRec[Option[(TypeRepr, TypeRepr)]] = {
      def parts(t: TypeRepr): TailRec[Option[(TypeRepr, TypeRepr)]] = normalize(t) match {
        case DirectCons(head, tail) => done(Some((head, tail)))
        case OrType(a, b) => tailcall(unionCons(a, b))
        case other => done(baseCons(other))
      }
      tailcall(parts(a)).flatMap {
        case Some((ha, ta)) =>
          // Carry both tails forward without asking the compiler to compare their entire common prefix.
          tailcall(parts(b)).map(_.map { case (hb, tb) => (union(ha, hb), OrType(ta, tb)) })
        case None => done(None)
      }
    }

    private inline def undecidedInUnion(t: TypeRepr, undecided: => Boolean): Boolean =
      t match {
        // Equal tails, including differently spelled aliases, behave like a single list. Check equality only
        // when an undecided head would otherwise stop the walk.
        case OrType(a, b) => undecided && !(a =:= b)
        case _ => false
      }

    private def isDirectCons(t: TypeRepr): Boolean = {
      @annotation.tailrec def loop(next: TypeRepr, pending: List[TypeRepr]): Boolean = next match {
        case a: AppliedType if isCons(a) => pending match {
          case scala.collection.immutable.::(t, rest) => loop(normalize(t), rest)
          case Nil => true
        }
        case OrType(a, b) => loop(normalize(a), b :: pending)
        case _ => false
      }
      loop(t, Nil)
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
    private final case class Branch(tpe: TypeRepr, revInit: List[TypeRepr], bounded: Boolean, expanded: List[TypeRepr])

    @annotation.tailrec
    private def go(
      rest: TypeRepr,
      revInit: List[TypeRepr],
      mode: Mode,
      bounded: Boolean,
      expanded: List[TypeRepr],
      pending: List[Branch],
    ): Either[LookupError, Outcome] = {
      steps += 1
      // A tail that had to be reduced, e.g. an alias or a match type, may grow as it is reduced again, so these have a
      // smaller budget, stopping before the compiler's own recursion limit. A union of `H :: T`s doesn't grow as it's
      // walked, so only other tails count.
      // Most tails are already `H :: T`, which needs no normalizing or separate check of the result.
      val spelledCons = isCons(rest)
      if (!spelledCons && !isDirectCons(rest)) expansions += 1
      val t = normalize(rest)
      val direct = spelledCons || isDirectCons(t)
      if (steps > Lookup.MaxSteps) Left(LookupError.Refused(s"look through $record", Reason.TooLong(Lookup.MaxSteps)))
      else if (expansions > Lookup.MaxExpansions)
        Left(LookupError.Refused(s"look through $record", Reason.TooManyReductions(Lookup.MaxExpansions)))
      else if (!direct && expandedBefore(t, expanded)) Right(Unknown(t))
      else consParts(t) match {
        case Some((head, tail)) =>
          // An opaque record's elements are read off its bound, e.g. `Any` for `opaque type O <: Any :: HNil = Int :: HNil`
          val nowBounded = bounded || (!direct && (!isGround(t) || t.typeSymbol.flags.is(Flags.Opaque)))
          val differentKey = keyMismatch(head)
          if (!differentKey && cmpHolds(head)) Right(Found(revInit, head, tail)(rest, nowBounded))
          else {
            val refusal =
              if (differentKey || target == Target.Element) None
              else strictFieldSubtype(head).map(n => Reason.StrictFieldSubtype(display(n), revInit.length))
                .orElse(if (target == Target.UpdateField) updateConflict(head, revInit.length, nowBounded) else None)
            refusal match {
              case Some(reason) =>
                val action = if (target == Target.UpdateField) s"update $record with $field" else s"find field $field in record $record"
                Left(LookupError.Refused(action, reason))
              case None =>
                // Different literal keys rule out a match unless reading a subtype off an abstract bound.
                lazy val undecided = (!differentKey || (nowBounded && !cmpIsEq)) && mightMatch(head, nowBounded)
                // An element of a union of lists that matches in only some arms, e.g. `String | Int` in
                // `(String :: Int :: HNil) | (Int :: Int :: HNil)` looked up by `Int`, is the first match in those
                // arms, so no later element is the first match in all of them.
                if ((mode == Mode.Prove && undecided) || undecidedInUnion(t, undecided)) Right(Undecided(revInit, head))
                else {
                  val nowMode = mode match {
                    case Mode.Update(None) if undecided => Mode.Update(Some(Undecided(revInit, head)))
                    case same @ (Mode.Find | Mode.Prove | Mode.Update(_)) => same
                  }
                  go(tail, head :: revInit, nowMode, nowBounded, if (direct) expanded else t :: expanded, pending)
                }
            }
          }
        case None =>
          if (t <:< hnilTpe) pending match {
            case scala.collection.immutable.::(branch, rest) =>
              go(branch.tpe, branch.revInit, Mode.Prove, branch.bounded, branch.expanded, rest)
            case Nil => Right(mode match {
              case Mode.Update(Some(first)) => first
              case Mode.Find | Mode.Prove | Mode.Update(None) => Absent
            })
          }
          else t match {
            // Some arms can end before others. Absence must hold in every arm.
            case OrType(a, b) if mode == Mode.Prove =>
              go(a, revInit, Mode.Prove, bounded, expanded, Branch(b, revInit, bounded, expanded) :: pending)
            case _ => Right(Unknown(t))
          }
      }
    }

    private def start(mode: Mode): Either[LookupError, Outcome] =
      go(TypeRepr.of[T], Nil, mode, bounded = false, expanded = Nil, pending = Nil)

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
        case a: AppliedType if isCons(a) => t
        case _: OrType => t
        case _ => t.dealias match {
          case a: AppliedType if isCons(a) => a
          case union: OrType => union
          case other => other.simplified
        }
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
        else go(f.tail, f.head :: f.revInit, Mode.Update(None), f.bounded, expanded = Nil, pending = Nil) match {
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
    def appended(t: TypeRepr, f: TypeRepr): Option[TypeRepr] = {
      @annotation.tailrec
      def loop(rest: TypeRepr, prefix: List[TypeRepr]): TailRec[Option[TypeRepr]] =
        rest.dealias match {
          case DirectCons(head, tail) => loop(tail, head :: prefix)
          case OrType(a, b) => combine(a, b, prefix)
          case end if end =:= hnilTpe => done(Some(prefix.foldLeft(cons(f, end))((acc, h) => cons(h, acc))))
          case _ => done(None)
        }
      def combine(a: TypeRepr, b: TypeRepr, prefix: List[TypeRepr]): TailRec[Option[TypeRepr]] =
        tailcall(loop(a, Nil)).flatMap {
          case Some(x) =>
            tailcall(loop(b, Nil)).map(_.map(y => prefix.foldLeft(OrType(x, y): TypeRepr)((acc, h) => cons(h, acc))))
          case None => done(None)
        }
      loop(t, Nil).result
    }

    /**
     * `T` without `found` in each arm, preserving the types that occur together in that arm. Reinsertion is sound
     * only when every prefix is spelled out as cons cells and every arm has exactly `F` at the chosen position.
     */
    def removedReinsertably(found: Found): Either[LookupError, TypeRepr] = {
      def refuse(reason: Reason): Left[LookupError, Nothing] = Left(LookupError.Refused(s"remove $field from $record", reason))

      @annotation.tailrec
      def loop(t: TypeRepr, remaining: Int, prefix: List[TypeRepr]): TailRec[Either[LookupError, TypeRepr]] =
        normalize(t) match {
          case OrType(a, b) => combine(a, b, remaining, prefix)
          case DirectCons(head, tail) =>
            if (remaining > 0) loop(tail, remaining - 1, head :: prefix)
            else if (head =:= fTpe) done(Right(prefix.foldLeft(tail)((acc, h) => cons(h, acc))))
            else done(refuse(Reason.ArmDiffers(display(head), found.index, field)))
          case other => done(refuse(Reason.NotSpelledOut(display(other))))
        }
      def combine(a: TypeRepr, b: TypeRepr, remaining: Int, prefix: List[TypeRepr]): TailRec[Either[LookupError, TypeRepr]] =
        tailcall(loop(a, remaining, Nil)).flatMap {
          case Right(x) =>
            tailcall(loop(b, remaining, Nil)).map(_.map(y => prefix.foldLeft(OrType(x, y): TypeRepr)((acc, h) => cons(h, acc))))
          case Left(error) => done(Left(error))
        }
      loop(TypeRepr.of[T], found.index, Nil).result
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
    val lookup = Lookup.Walk[T, F, Cmp](Lookup.Target.Element)
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
    materialize[T, F, Op, FieldOp[T, F, Op]](
      [O] => (out: Type[O], index: Int) => '{ new Inst[T, F, Op, out.Underlying](${ Expr(index) }) },
    )

  private[formless] def materialize[T <: HList: Type, F: Type, Op <: FieldOp.Op: Type, E](
    build: [O] => (Type[O], Int) => Expr[E],
  )(using Quotes): Expr[E] =
    Lookup.guarded("look up a record field")(fieldResult[T, F, Op, E](build))

  private def fieldResult[T <: HList: Type, F: Type, Op <: FieldOp.Op: Type, E](
    build: [O] => (Type[O], Int) => Expr[E],
  )(using Quotes): Either[Lookup.LookupError, Expr[E]] = {
    val lookup = Lookup.Walk[T, F, <:<](Lookup.Target.Element)
    for {
      f <- lookup.found
      kv <- lookup.fieldKeyValue(f)
      result <- Type.of[Op] match {
        case '[Select] =>
          (kv._2.asType: @unchecked) match {
            case '[v] => Right(build[v](Type.of[v], f.index))
          }
        case '[Remove] =>
          ((kv._2.asType, lookup.removed(f).asType): @unchecked) match {
            case ('[v], '[rem]) => Right(build[(v, rem)](Type.of[(v, rem)], f.index))
          }
        case _ => Left(Lookup.LookupError.AbstractOperation(Type.show[Op]))
      }
    } yield result
  }

  transparent inline given fieldOpInst[T <: HList, F, Op <: FieldOp.Op]: FieldOp[T, F, Op] = ${ fieldOpImpl[T, F, Op] }
}

/**
 * How `Updater` puts the field `F` into the record `T`: it replaces the first field of that key if it is `=:=` to
 * `F`, or appends `F` if the key is absent. `Out` is the new record.
 */
@scala.annotation.implicitNotFound("Cannot update ${T} with ${F}")
private[formless] sealed trait FieldUpdate[T <: HList, F] extends Updater[T, F]

object FieldUpdate {
  private[formless] final class Replace[T <: HList, F, O](index: Int) extends FieldUpdate[T, F] {
    final type Out = O
    def apply(t: T, f: F): O = HList.setAt(t, index, f).asInstanceOf[O]
  }

  private[formless] final class Append[T <: HList, F, O] extends FieldUpdate[T, F] {
    final type Out = O
    def apply(t: T, f: F): O = HList.append(t, f).asInstanceOf[O]
  }

  private def fieldUpdateImpl[T <: HList: Type, F: Type](using Quotes): Expr[FieldUpdate[T, F]] =
    Lookup.guarded("update a record field")(fieldUpdateImpl0[T, F])

  private def fieldUpdateImpl0[T <: HList: Type, F: Type](using Quotes): Either[Lookup.LookupError, Expr[FieldUpdate[T, F]]] = {
    val lookup = Lookup.Walk[T, F, =:=](Lookup.Target.UpdateField)
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
 * The first element of `T` that is `=:=` to `F`, with `T` as a type lambda `Replaced` of what replaces it.
 * Unlike `FieldOp`, the replacement isn't one of its type arguments, so `Modifier` and `Replacer` can leave the new
 * value's type to be inferred, e.g. from the expected result type, after the element is found.
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
  /** `F` is an HList element type */
  sealed trait Elem extends Kind

  private[formless] final class Inst[T <: HList, F, Kind <: ReplaceAt.Kind, R[_] <: HList](val index: Int) extends ReplaceAt[T, F, Kind] {
    final type Replaced[A] = R[A]
  }

  private def replaceAtImpl[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type](using Quotes): Expr[ReplaceAt[T, F, Kind]] =
    Lookup.guarded("find an element to replace")(replaceAtImpl0[T, F, Kind])

  // `Kind`'s bound limits it to these two kinds; the last case is an abstract `Kind`, which can't be expanded.
  private def replaceAtImpl0[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type](using Quotes): Either[Lookup.LookupError, Expr[ReplaceAt[T, F, Kind]]] =
    Type.of[Kind] match {
      case '[Field] => replaceAtWith[T, F, Kind](Lookup.Target.ExactField)
      case '[Elem] => replaceAtWith[T, F, Kind](Lookup.Target.Element)
      case _ => Left(Lookup.LookupError.AbstractOperation(Type.show[Kind]))
    }

  private def replaceAtWith[T <: HList: Type, F: Type, Kind <: ReplaceAt.Kind: Type](target: Lookup.Target)(
    using Quotes,
  ): Either[Lookup.LookupError, Expr[ReplaceAt[T, F, Kind]]] = {
    val walk = Lookup.Walk[T, F, =:=](target)
    import walk.q.reflect.*

    for {
      f <- walk.found
      _ <- target match {
        case Lookup.Target.ExactField | Lookup.Target.UpdateField => walk.fieldKeyValue(f).map(_ => ())
        case Lookup.Target.Element => Right(())
      }
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
private[formless] opaque type MaybeRecord[T <: HList] = Unit

object MaybeRecord {
  private[formless] inline def proof[T <: HList]: MaybeRecord[T] = ()

  private def maybeRecordImpl[T <: HList: Type](using Quotes): Expr[MaybeRecord[T]] =
    Lookup.guarded("check for record fields")(maybeRecordImpl0[T])

  private def maybeRecordImpl0[T <: HList: Type](using Quotes): Either[Lookup.LookupError, Expr[MaybeRecord[T]]] = {
    val lookup = Lookup.Walk[T, Nothing, <:<](Lookup.Target.Element)
    if (lookup.isPlainList) Left(Lookup.LookupError.PlainList(lookup.display(lookup.q.reflect.TypeRepr.of[T])))
    else Right('{ proof[T] })
  }

  transparent inline given maybeRecordInst[T <: HList]: MaybeRecord[T] = ${ maybeRecordImpl[T] }
}

/** Never resolves: a record's `replace` takes a key and a value */
@scala.annotation.implicitNotFound("A record's replace takes a key and a value, e.g. record.replace(\"key\", value)")
private[formless] sealed trait ReplaceTakesAKey[T <: HList] {
  type Out
  def out: Out
}

