package formless.hlist

import compiletime.ops.int.{-, >=, S}
import scala.language.implicitConversions

/**
 * `HList` ADT base trait.
 */
sealed trait HList extends Product with Serializable {
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

  type FromTuple[T <: Tuple] <: HList = T match {
    case EmptyTuple => HNil
    case h *: t => h :: FromTuple[t]
  }

  def fromTuple[T <: Tuple](t: T): FromTuple[T] = t match {
    case _: EmptyTuple => HNil
    case x: (h *: t) => x.head.asInstanceOf[h] :: fromTuple(x.tail.asInstanceOf[t])
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

  type Fill[N <: Int, A] <: HList = N match {
    case 0 => HNil
    case _ => A :: Fill[N - 1, A]
  }

  @annotation.tailrec
  private[formless] def fill0[N <: Int, A](n: N, a: A, acc: HList): HList =
    if (n <= 0) acc
    else fill0(n - 1, a, a :: acc)

  /**
   * Produces an `HList` of length `n` filled with `elem`.
   */
  def fill[A](n: Int)(a: A)(using ev: (n.type >= 0) =:= true): Fill[n.type, A] =
    fill0(n, a, HNil).asInstanceOf[Fill[n.type, A]]

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

  private[formless] def drop0[H <: HList, N <: Int](h: H, n: N): Drop[H, N] =
    (n match {
      case 0 => h
      case _ => h match {
        case x @ HNil => x
        case _ :: t => drop0(t, n - 1)
      }
    }).asInstanceOf[Drop[H, N]]

  def drop[H <: HList, N <: Int](h: H, n: N)(using ev: (Size[H] >= N) =:= true): Drop[H, N] = drop0(h, n)

  type Elem[H <: HList, N <: Int] = H match {
    case h :: t =>
      N match {
        case 0 => h
        case S[n] => Elem[t, n]
      }
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

  private[formless] def take0[H <: HList, N <: Int](h: H, n: N): Take[H, N] =
    (n match {
      case 0 => HNil
      case _ => h match {
        case x @ HNil => x
        case h :: t => h :: take0(t, n - 1)
      }
    }).asInstanceOf[Take[H, N]]

  def take[H <: HList, N <: Int](h: H, n: N)(using ev: (Size[H] >= N) =:= true): Take[H, N] = take0(h, n)
}
