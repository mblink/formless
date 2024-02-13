package formless.record

import scala.deriving.Mirror
import formless.tuple.ZipWithT

trait LabelledGeneric[A] extends LabelledGeneric.Inst[A]

object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  inline def apply[A](using l: LabelledGeneric[A]): LabelledGeneric.Aux[A, l.Repr] = l
  def toRecord[A](a: A)(using l: LabelledGeneric[A]): l.Repr = l.to(a)

  given fromInst[A](using i: Inst[A]): LabelledGeneric.Aux[A, i.Repr] =
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

    inline given productInst[A <: Product](
      using m: Mirror.ProductOf[A]
    ): Inst.Aux[A, ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]] =
      new Inst[A] {
        type Repr = Tuple & ZipWithT[m.MirroredElemLabels, m.MirroredElemTypes, ->>]
        def from(r: Repr): A = m.fromTuple(r.asInstanceOf[m.MirroredElemTypes])
        def to(a: A): Repr = Tuple.fromProductTyped(a).asInstanceOf[Repr]
      }
  }
}
