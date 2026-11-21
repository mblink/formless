package formless.hlist

import compiletime.ops.boolean.&&
import compiletime.ops.int.{-, <, >=, S}
import scala.language.implicitConversions

/**
 * `HList` ADT base trait.
 */
sealed trait HList extends Product, Serializable {
  def runtimeLength: Int

  private[formless] final def toArray: Array[Any] = {
    val arr = new Array[Any](runtimeLength)

    @annotation.tailrec def loop[H <: HList](h: H, i: Int): Unit =
      h match {
        case HNil => ()
        case h :: t =>
          arr(i) = h
          loop(t, i + 1)
      }

    loop(this, 0)
    arr
  }

  private[formless] final def unsafeApply(idx: Int): Any = {
    @annotation.tailrec def loop[H <: HList](h: H, i: Int): Any =
      h match {
        case HNil => throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${runtimeLength - 1})")
        case h :: t =>
          if (i == 0) h
          else loop(t, i - 1)
      }

    loop(this, idx)
  }
}

sealed trait NonEmptyHList extends HList

/**
 * Non-empty `HList` element type.
 */
final case class ::[+H, +T <: HList](head: H, tail: T) extends NonEmptyHList {
  override final lazy val runtimeLength = 1 + tail.runtimeLength

  override final lazy val toString = head match {
    case _: ::[_, _] => s"($head) :: $tail"
    case _ => s"$head :: $tail"
  }
}

/**
 * Empty `HList` value.
 */
case object HNil extends HList {
  override final lazy val runtimeLength = 0
}
type HNil = HNil.type

/**
 * Summon the given `HList` of constant value types
 */
inline def constValueHList[H <: HList]: H =
  (inline compiletime.erasedValue[H] match {
    case _: HNil => HNil
    case _: (h :: t) => compiletime.constValue[h] :: constValueHList[t]
  }).asInstanceOf[H]

private[formless] inline def summonAllHList0[H <: HList](f: Any => Any): HList =
  inline compiletime.erasedValue[H] match {
    case _: HNil => HNil
    case _: (h :: t) => f(compiletime.summonInline[h]) :: summonAllHList0[t](f)
  }

inline def summonAllValueOfHList[H <: HList]: H =
  summonAllHList0[HList.Map[H, ValueOf]](_.asInstanceOf[ValueOf[?]].value).asInstanceOf[H]

inline def summonAllHList[H <: HList]: H =
  summonAllHList0[H](identity).asInstanceOf[H]

object HList {
  implicit def toHListOps[H <: HList](h: H): HListOps[H] = new HListOps(h)

  def apply() = HNil

  def fromArray[A](a: Array[A]): HList = a.foldRight(HNil: HList)(_ :: _)

  /**
   * The elements of `l` before index `idx`, and the rest of `l` from `idx` on, which is shared rather than copied so a
   * singleton tail type like `t.type` still describes it. An array and a loop rather than immutable lists, since this
   * runs on every record and HList update. `idx` comes from a lookup macro that has checked it against `l`'s type.
   */
  private def splitAt(l: HList, idx: Int): (Array[Any], HList) = {
    val prefix = new Array[Any](idx)

    @annotation.tailrec def loop(h: HList, i: Int): HList =
      if (i == idx) h
      else h match {
        case c: ::[Any, HList] @unchecked =>
          prefix(i) = c.head
          loop(c.tail, i + 1)
        case HNil => throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${l.runtimeLength})")
      }

    (prefix, loop(l, 0))
  }

  private def prependAll(prefix: Array[Any], rest: HList): HList = prefix.foldRight(rest)(_ :: _)

  /** The element at `idx` and the elements before it, with the rest of `l` after it */
  private def cellAt(l: HList, idx: Int): (Array[Any], ::[Any, HList]) =
    splitAt(l, idx) match {
      case (prefix, c: ::[Any, HList] @unchecked) => (prefix, c)
      case _ => throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${l.runtimeLength - 1})")
    }

  /**
   * `l` with `f` applied to its element at `idx`; the elements after it are shared. The lookup that found `idx` proved
   * the element is an `A`, which the erased list can't show, so this is the one place that reads it as one.
   */
  private[formless] def updatedAt[A](l: HList, idx: Int, f: A => Any): HList = {
    val (prefix, c) = cellAt(l, idx)
    prependAll(prefix, f(c.head.asInstanceOf[A]) :: c.tail)
  }

  private def reversed(prefix: Array[Any]): HList = prefix.foldLeft(HNil: HList)((acc, x) => x :: acc)

  /** The elements of `l` before index `idx`, and the rest of `l` from `idx` on, which is shared */
  private[formless] def splitBefore(l: HList, idx: Int): (HList, HList) = {
    val (prefix, rest) = splitAt(l, idx)
    (prependAll(prefix, HNil), rest)
  }

  /** The elements of `l` before index `idx` in reverse, and the rest of `l` from `idx` on, which is shared */
  private[formless] def reverseSplitBefore(l: HList, idx: Int): (HList, HList) = {
    val (prefix, rest) = splitAt(l, idx)
    (reversed(prefix), rest)
  }

  /** The elements of `l` up to and including index `idx`, and the rest of `l` after it, which is shared */
  private[formless] def splitAfter(l: HList, idx: Int): (HList, HList) = {
    val (prefix, c) = cellAt(l, idx)
    (prependAll(prefix, c.head :: HNil), c.tail)
  }

  /**
   * The element at index `idx` followed by the elements before it in reverse, and the rest of `l` after it, which is
   * shared
   */
  private[formless] def reverseSplitAfter(l: HList, idx: Int): (HList, HList) = {
    val (prefix, c) = cellAt(l, idx)
    (c.head :: reversed(prefix), c.tail)
  }

  /** The element at `idx`, read as an `A` as in `updatedAt`, and `l` with `f` of it in its place */
  private[formless] def replacedAt[A](l: HList, idx: Int, f: A => Any): (A, HList) = {
    val (prefix, c) = cellAt(l, idx)
    val a = c.head.asInstanceOf[A]
    (a, prependAll(prefix, f(a) :: c.tail))
  }

  /** `l` with `x` inserted at index `idx`, which may be `l`'s length; the elements from `idx` on are shared */
  private[formless] def insertedAt(l: HList, idx: Int, x: Any): HList = {
    val (prefix, rest) = splitAt(l, idx)
    prependAll(prefix, x :: rest)
  }

  /** The element at `idx` and `l` without it; the elements after it are shared */
  private[formless] def removedAt(l: HList, idx: Int): (Any, HList) = {
    val (prefix, c) = cellAt(l, idx)
    (c.head, prependAll(prefix, c.tail))
  }

  type Head[H <: HList] = H match { case h :: _ => h }
  type Tail[H <: HList] = H match { case _ :: t => t }

  type FromTuple[T <: Tuple] <: HList = T match {
    case EmptyTuple => HNil
    case h *: t => h :: FromTuple[t]
  }

  def fromTuple[T <: Tuple](t: T): FromTuple[T] = t match {
    case _: EmptyTuple => HNil
    case x: (h *: t) =>
      val tuple: h *: t = x
      tuple.head :: fromTuple(tuple.tail)
  }

  type ToTuple[H <: HList] <: Tuple = H match {
    case HNil => EmptyTuple
    case h :: t => h *: ToTuple[t]
  }

  def toTuple[H <: HList](h: H): ToTuple[H] = h match {
    case _: HNil => EmptyTuple
    case x: (_ :: _) => x.head *: toTuple(x.tail)
  }

  type ToUnion[H <: HList] = H match {
    case HNil => Nothing
    case h :: t => h | ToUnion[t]
  }

  type Length[H <: HList] <: Int = H match {
    case HNil => 0
    case _ :: t => S[Length[t]]
  }

  def length[H <: HList](h: H): Length[H] = h match {
    case _: HNil => 0
    case x: (_ :: t) => (1 + length[t](x.tail)).asInstanceOf[S[Length[t]]]
  }

  type Fill[N <: Int, A] <: HList = N match {
    case 0 => HNil
    case _ => A :: Fill[N - 1, A]
  }

  private[formless] type FillConcat[N <: Int, A, Acc <: HList] <: HList = Acc match {
    case HNil => Fill[N, A]
    case _ => Concat[Fill[N, A], Acc]
  }

  @annotation.tailrec
  private[formless] def fill0[N <: Int, A, H <: HList](n: N, a: A, acc: H): FillConcat[N, A, H] =
    (if (n <= 0) acc else fill0(n - 1, a, a :: acc)).asInstanceOf[FillConcat[N, A, H]]

  /**
   * Produces an `HList` of length `n` filled with `elem`.
   */
  def fill[A](n: Int)(a: A)(using ev: (n.type >= 0) =:= true): Fill[n.type, A] =
    fill0[n.type, A, HNil](n, a, HNil)

  type Map[H <: HList, F[_]] <: HList = H match {
    case HNil => HNil
    case h :: t => F[h] :: Map[t, F]
  }

  type InverseMap[H <: HList, F[_]] <: HList = H match {
    case HNil => HNil
    case F[h] :: t => h :: InverseMap[t, F]
  }

  type IsMappedBy[F[_]] = [H <: HList] =>> H =:= Map[InverseMap[H, F], F]

  type Append[H <: HList, A] <: HList = H match {
    case HNil => A :: HNil
    case h :: t => h :: Append[t, A]
  }

  def append[H <: HList, A](h: H, a: A): Append[H, A] = h match {
    case h: HNil => a :: h
    case x: (_ :: _) => x.head :: append(x.tail, a)
  }

  type Concat[X <: HList, +Y <: HList] <: HList = X match {
    case HNil => Y
    case h :: t => h :: Concat[t, Y]
  }

  def concat[X <: HList, Y <: HList](x: X, y: Y): Concat[X, Y] = x match {
    case _: HNil => y
    case x: (_ :: _) => x.head :: concat(x.tail, y)
  }

  /** Transforms an `HList` `(T1, ..., Tn)` into `(Ti+1, ..., Tn)`. */
  type Drop[H <: HList, N <: Int] <: HList = N match {
    case 0 => H
    case S[n] => H match {
      case HNil => HNil
      case _ :: xs => Drop[xs, n]
    }
  }

  private def drop0[H <: HList, N <: Int](h: H, n: N): Drop[H, N] =
    (n match {
      case 0 => h
      case _ => h match {
        case x @ HNil => x
        case _ :: t => drop0(t, n - 1)
      }
    }).asInstanceOf[Drop[H, N]]

  def drop[H <: HList, N <: Int](h: H, n: N)(using CountInBounds[H, N]): Drop[H, N] = drop0(h, n)

  type Elem[H <: HList, N <: Int] = H match {
    case h :: t =>
      N match {
        case 0 => h
        case S[n] => Elem[t, n]
      }
  }

  @scala.annotation.implicitNotFound("Cannot access index ${N} of ${H}. Supply a singleton Int index between zero and the HList length minus one.")
  private[formless] final class Index[H <: HList, N <: Int & Singleton] private (val value: Int) extends AnyVal

  private[formless] object Index {
    given inBounds[H <: HList, N <: Int & Singleton](
      using n: ValueOf[N],
      ev: ((N >= 0) && (N < Size[H])) =:= true,
    ): Index[H, N] = new Index[H, N](n.value)
  }

  type Init[H <: NonEmptyHList] <: HList = H match {
    case _ :: HNil => HNil
    case h :: t => h :: Init[t]
  }

  private def init0[H <: NonEmptyHList](h: H): Init[H] =
    (h match {
      case _ :: HNil => HNil
      case h :: (t @ (_ :: _)) => h :: init0(t)
    }).asInstanceOf[Init[H]]

  def init[H <: NonEmptyHList](h: H): Init[H] = init0(h)

  type Last[H <: NonEmptyHList] = H match {
    case h :: HNil => h
    case _ :: t => Last[t]
  }

  private def last0[H <: NonEmptyHList](h: H): Last[H] =
    (h match {
      case h :: HNil => h
      case _ :: (t @ (_ :: _)) => last0(t)
    }).asInstanceOf[Last[H]]

  def last[H <: NonEmptyHList](h: H): Last[H] = last0(h)

  type Reverse[H <: HList] <: HList = H match {
    case HNil => HNil
    case h :: t => Append[Reverse[t], h]
  }

  def reverse[H <: HList](h: H): Reverse[H] = h match {
    case _: HNil => HNil
    case x: (h :: t) => append[Reverse[t], h](reverse(x.tail), x.head)
  }

  type Size[H <: HList] <: Int = H match {
    case HNil => 0
    case _ :: t => S[Size[t]]
  }

  private[formless] type CountInBounds[H <: HList, N <: Int] = ((N >= 0) && (Size[H] >= N)) =:= true

  def size[H <: HList](h: H): Size[H] = h match {
    case _: HNil => 0
    case x: (_ :: t) => (size(x.tail) + 1).asInstanceOf[S[Size[t]]]
  }

  type Take[H <: HList, N <: Int] <: HList = N match {
    case 0 => HNil
    case S[n] => H match {
      case HNil => HNil
      case x :: xs => x :: Take[xs, n]
    }
  }

  private def take0[H <: HList, N <: Int](h: H, n: N): Take[H, N] =
    (n match {
      case 0 => HNil
      case _ => h match {
        case x @ HNil => x
        case h :: t => h :: take0(t, n - 1)
      }
    }).asInstanceOf[Take[H, N]]

  def take[H <: HList, N <: Int](h: H, n: N)(using CountInBounds[H, N]): Take[H, N] = take0(h, n)
}
