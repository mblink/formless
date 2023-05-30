package typify
package tuple

import scala.language.implicitConversions

final class TupleOps[T <: Tuple](private val t: T) extends AnyVal {
  final def *:[H](h: H): H *: T = shapeless.::(h, t)
}

private[typify] trait TuplePackageAux {
  final type Tuple = shapeless.HList
  final type *:[H, T <: Tuple] = shapeless.::[H, T]
  final type EmptyTuple = shapeless.HNil
  final val EmptyTuple: EmptyTuple = shapeless.HNil

  @inline final implicit def tupleToTupleOps[T <: Tuple](t: T): TupleOps[T] =
    new TupleOps[T](t)

  final type IsNonEmptyTuple[T <: Tuple] = shapeless.ops.hlist.IsHCons[T]
  final val IsNonEmptyTuple: shapeless.ops.hlist.IsHCons.type = shapeless.ops.hlist.IsHCons

  final type LiftAll[F[_], In <: Tuple] = shapeless.ops.hlist.LiftAll[F, In]
  final val LiftAll: shapeless.ops.hlist.LiftAll.type = shapeless.ops.hlist.LiftAll

  final type MapFolder[T <: Tuple, R, F] = shapeless.ops.hlist.MapFolder[T, R, F]
  final val MapFolder: shapeless.ops.hlist.MapFolder.type = shapeless.ops.hlist.MapFolder

  final type Prepend[L <: Tuple, R <: Tuple] = shapeless.ops.hlist.Prepend[L, R]
  final val Prepend: shapeless.ops.hlist.Prepend.type = shapeless.ops.hlist.Prepend

  final type Reify[T <: Tuple] = shapeless.ops.hlist.Reify[T]
  final val Reify: shapeless.ops.hlist.Reify.type = shapeless.ops.hlist.Reify

  final type RightFolder[L <: Tuple, In, F] = shapeless.ops.hlist.RightFolder[L, In, F]
  final val RightFolder: shapeless.ops.hlist.RightFolder.type = shapeless.ops.hlist.RightFolder

  final type ToList[T <: Tuple, Lub] = shapeless.ops.hlist.ToList[T, Lub]

  final type TupleSelector[T <: Tuple, A] = shapeless.ops.hlist.Selector[T, A]
  final val TupleSelector: shapeless.ops.hlist.Selector.type = shapeless.ops.hlist.Selector

  final type DepFn0 = shapeless.DepFn0
  final type DepFn1[T] = shapeless.DepFn1[T]
  final type DepFn2[T, U] = shapeless.DepFn2[T, U]

  trait Poly0 extends shapeless.Poly {
    final type Case[A] = shapeless.PolyDefns.Case0.Aux[this.type, A]
    @inline final def at[A](f: () => A): Case[A] =
      new shapeless.PolyDefns.Case0[this.type] {
        type Result = A
        val value = (_: EmptyTuple) => f()
      }
  }
  trait Poly1 extends shapeless.Poly {
    final type Case[A, B] = shapeless.PolyDefns.Case1.Aux[this.type, A, B]
    @inline final def at[A, B](f: A => B): Case[A, B] =
      new shapeless.PolyDefns.Case1[this.type, A] {
        type Result = B
        val value = (l: A *: EmptyTuple) => f(l.head)
      }
  }
  trait Poly2 extends shapeless.Poly {
    final type Case[A, B, C] = shapeless.PolyDefns.Case2.Aux[this.type, A, B, C]
    @inline final def at[A, B, C](f: (A, B) => C): Case[A, B, C] =
      new shapeless.PolyDefns.Case2[this.type, A, B] {
        type Result = C
        val value = (l: A *: B *: EmptyTuple) => f(l.head, l.tail.head)
      }
  }
}
