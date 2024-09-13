package formless.record

import scala.deriving.Mirror
import formless.hlist.HList

trait LabelledGeneric[A] extends LabelledGeneric.Inst[A]

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: LabelledGeneric[A]): LabelledGeneric.Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: LabelledGeneric[A]): l.Repr = l.to(a)

  given fromInst[A, R](using i: Inst[A]): LabelledGeneric.Aux[A, i.Repr] =
    new LabelledGeneric[A] {
      type Repr = i.Repr
      def from(r: Repr): A = i.from(r)
      def to(a: A): Repr = i.to(a)
    }

  sealed trait Inst[A] extends Serializable {
    type Repr
    def from(r: Repr): A
    def to(a: A): Repr
  }

  object Inst {
    type Aux[A, R] = Inst[A] { type Repr = R }

    type ZipWithTuple[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
      case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWithTuple[t1, t2, F]
      case (EmptyTuple, EmptyTuple) => EmptyTuple
    }

    given productInst[A <: Product](
      using m: Mirror.ProductOf[A]
    ): Inst.Aux[A, HList.FromTuple[ZipWithTuple[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]] =
      new Inst[A] {
        type Repr = HList.FromTuple[ZipWithTuple[m.MirroredElemLabels, m.MirroredElemTypes, ->>]]
        def from(r: Repr): A = m.fromTuple(HList.toTuple(r).asInstanceOf[m.MirroredElemTypes])
        def to(a: A): Repr = HList.fromTuple(Tuple.fromProductTyped(a)).asInstanceOf[Repr]
      }
  }
}
