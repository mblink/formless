package formless.tuple

import scala.language.implicitConversions

final class FillWithOps[L](private val dummy: Boolean = false) extends AnyVal {
  final def apply[F](f: F)(using fw: FillWith[F, L]): L = fw()
}

trait TuplePackageCompat {
  final type Tuple = scala.Tuple
  final type *:[H, T <: Tuple] = scala.*:[H, T]
  final val *: : scala.*:.type = scala.*:
  final type EmptyTuple = scala.EmptyTuple
  final val EmptyTuple: EmptyTuple = scala.EmptyTuple

  @inline final implicit def toFormlessTupleOps[L <: Tuple](l: L): FormlessTupleOps[L] = new FormlessTupleOps[L](l)

  extension(t: Tuple.type) {
    /**
     * Produces a `Tuple` of length `n` filled with `elem`.
     */
    final def fill[A](n: Int)(elem: A)(using f: Fill[n.type, A]): f.Out = f(elem)
    /**
     * Produces an `n1`-length `Tuple` made of `n2`-length `Tuple`s filled with `elem`.
     */
    final def fill[A](n1: Int, n2: Int)(elem: A)(using f: Fill[(n1.type, n2.type), A]): f.Out = f(elem)
    /**
      * Produces a `Tuple` filled from a `Poly0`.
      */
    final def fillWith[L]: FillWithOps[L] = new FillWithOps[L]
  }
}
