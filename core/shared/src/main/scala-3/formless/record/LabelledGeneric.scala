package formless.record

import scala.deriving.Mirror
import formless.hlist.HList

trait LabelledGeneric[A] extends Serializable {
  type Repr
  def to(a: A): Repr
  def from(r: Repr): A
}

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: LabelledGeneric[A]): LabelledGeneric.Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: LabelledGeneric[A]): l.Repr = l.to(a)

  given fromInst[A, R](using i: Inst[A, R]): LabelledGeneric.Aux[A, R] =
    new LabelledGeneric[A] {
      type Repr = R
      def from(r: Repr): A = i.from(r)
      def to(a: A): Repr = i.to(a)
    }

  final class Inst[A, R](val from: R => A, val to: A => R) extends Serializable

  object Inst {
    type ZipWithTuple[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
      case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWithTuple[t1, t2, F]
      case (EmptyTuple, EmptyTuple) => EmptyTuple
    }

    inline given productInst[A <: Product](
      using m: Mirror.ProductOf[A]
    ): Inst[A, HList.FromTuple[ZipWithTuple[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]] =
      Inst(
        r => m.fromTuple(HList.toTuple(r).asInstanceOf[m.MirroredElemTypes]),
        a => HList.fromTuple(Tuple.fromProductTyped(a))
          .asInstanceOf[HList.FromTuple[ZipWithTuple[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]],
      )
  }
}
