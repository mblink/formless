package formless.hlist

import scala.deriving.Mirror

trait Generic[A] extends Serializable {
  type Repr
  def to(a: A): Repr
  def from(r: Repr): A
}

object Generic {
  type Aux[A, R] = Generic[A] { type Repr = R }

  inline def apply[A](using g: Generic[A]): Generic.Aux[A, g.Repr] = g

  given fromInst[A, R](using i: Inst[A, R]): Generic.Aux[A, R] =
    new Generic[A] {
      type Repr = R
      def from(r: Repr): A = i.from(r)
      def to(a: A): Repr = i.to(a)
    }

  final class Inst[A, R](val from: R => A, val to: A => R) extends Serializable

  object Inst {
    inline given productInst[A <: Product](using m: Mirror.ProductOf[A]): Inst[A, HList.FromTuple[m.MirroredElemTypes]] =
      Inst(
        r => m.fromTuple(HList.toTuple(r).asInstanceOf[m.MirroredElemTypes]),
        a => HList.fromTuple(Tuple.fromProductTyped(a)),
      )
  }
}
