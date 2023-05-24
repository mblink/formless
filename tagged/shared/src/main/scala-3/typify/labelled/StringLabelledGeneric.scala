package typify.labelled

import scala.deriving.Mirror

private[typify] type ZipWith[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWith[t1, t2, F]
  case (EmptyTuple, ?) => EmptyTuple
  case (?, EmptyTuple) => EmptyTuple
  case _ => Tuple
}

trait StringLabelledGeneric[A] {
  type Repr
  def from(r: Repr): A
  def to(a: A): Repr
}

object StringLabelledGeneric {
  type Aux[A, R] = StringLabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: StringLabelledGeneric[A]): Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: StringLabelledGeneric[A]): l.Repr = l.to(a)

  implicit inline def inst[A <: Product](using
  m: Mirror.ProductOf[A]): Aux[A, ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]] =
    new StringLabelledGeneric[A] {
      type Repr = Tuple & ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]
      def from(r: Repr): A = m.fromTuple(r.asInstanceOf[m.MirroredElemTypes])
      def to(a: A): Repr = Tuple.fromProductTyped(a).asInstanceOf[Repr]
    }

  implicit inline def inst[A](using
  m: Mirror.SumOf[A]): Aux[A, Tuple.Union[ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]] =
    new StringLabelledGeneric[A] {
      type Repr = Tuple.Union[ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]
      def from(r: Repr): A = r.asInstanceOf[A]
      def to(a: A): Repr = a.asInstanceOf[Repr]
    }
}
