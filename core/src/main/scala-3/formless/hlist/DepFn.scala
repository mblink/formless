package formless.hlist

trait DepFn0 {
  type Out
  def apply(): Out
}

trait DepFn1[T] {
  type Out
  def apply(t: T): Out
}

trait DepFn2[T, U] {
  type Out
  def apply(t: T, u: U): Out
}

abstract class Case[F, L <: HList] extends Serializable {
  type Result
  val value: L => Result

  final def apply(t: L) = value(t)
  final def apply()(using ev: HNil =:= L) = value(HNil)
  final def apply[T](t: T)(using ev: (T :: HNil) =:= L) = value(t :: HNil)
  final def apply[T, U](t: T, u: U)(using ev: (T :: U :: HNil) =:= L) = value(t :: u :: HNil)
}

object Case {
  type Aux[F, L <: HList, R] = Case[F, L] { type Result = R }
  type Hom[F, T] = Case.Aux[F, T :: HNil, T]

  def apply[F, L <: HList, R](v: L => R): Case.Aux[F, L, R] =
    new Case[F, L] {
      type Result = R
      val value = v
    }
}

type Case0[F] = Case[F, HNil]
object Case0 {
  type Aux[F, R] = Case0[F] { type Result = R }

  def apply[F, R](r: => R): Case0.Aux[F, R] = Case(_ => r)
}

type Case1[F, A] = Case[F, A :: HNil]
object Case1 {
  type Aux[F, A, R] = Case1[F, A] { type Result = R }

  def apply[F, A, R](f: A => R): Case1.Aux[F, A, R] = Case { case (a :: HNil) => f(a) }
}

type Case2[F, A, B] = Case[F, A :: B :: HNil]
object Case2 {
  type Aux[F, A, B, R] = Case2[F, A, B] { type Result = R }

  def apply[F, A, B, R](f: (A, B) => R): Case2.Aux[F, A, B, R] = Case { case (a :: b :: HNil) => f(a, b) }
}

sealed trait Poly { self =>
  final def apply(using c: Case0[self.type]): c.Result = c()
  final def apply[A](a: A)(using c: Case1[self.type, A]): c.Result = c(a)
  final def apply[A, B](a: A, b: B)(using c: Case2[self.type, A, B]): c.Result = c(a, b)
}

trait Poly0 extends Poly { self =>
  final type Case0[R] = formless.hlist.Case0.Aux[self.type, R]

  final inline def at[R](r: => R): Case0[R] = formless.hlist.Case0[self.type, R](r)
}

trait Poly1 extends Poly { self =>
  final type Case[A] = Case1[this.type, A]
  object Case {
    final type Aux[A, R] = Case1.Aux[self.type, A, R]
  }

  final def at[A] = [R] => (f: A => R) => Case1[self.type, A, R](f)
}

trait Poly2 extends Poly { self =>
  final type Case[A, B] = Case2[this.type, A, B]
  object Case {
    final type Aux[A, B, R] = Case2.Aux[self.type, A, B, R]
  }

  final def at[A, B] = [R] => (f: (A, B) => R) => Case2[self.type, A, B, R](f)
}

/**
 * Base class for lifting a `Function1` to a `Poly1`
 */
class ->[T, R](f: T => R) extends Poly1 {
  final given subT[U <: T]: Case.Aux[U, R] = at(f)
}

trait LiftFunction1LP extends Poly1 {
  final given default[T]: Case.Aux[T, HNil] = at(_ => HNil)
}

/**
 * Base class for lifting a `Function1` to a `Poly1` over the universal domain, yielding a `HList` with the result as
 * its only element if the argument is in the original functions domain, `HNil` otherwise.
 */
class >->[T, R](f: T => R) extends LiftFunction1LP {
  final given subT[U <: T]: Case.Aux[U, R :: HNil] = at(f(_) :: HNil)
}

/**
 * Higher ranked function which converts `HLists` to tuples.
 */
object tupled extends Poly1 {
  given caseHList[L <: HList](using t: Tupler[L]): Case.Aux[L, t.Out] = at[L](t(_))
}

/**
 * Higher ranked function which converts products to `HLists`.
 */
object productElements extends Poly1 {
  given caseProduct[P](using g: Generic[P]): Case.Aux[P, g.Repr] = at[P](p => g.to(p))
}
