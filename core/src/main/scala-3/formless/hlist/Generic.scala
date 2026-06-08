package formless.hlist

import scala.deriving.Mirror

trait Generic[A] extends Generic.Inst[A]

object Generic {
  type Aux[A, R] = Generic[A] { type Repr = R }

  inline def apply[A](using g: Generic[A]): Generic.Aux[A, g.Repr] = g

  given fromInst[A](using i: Inst[A]): Generic.Aux[A, i.Repr] =
    new Generic[A] {
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

    given productInst[A <: Product](using m: Mirror.ProductOf[A]): Inst.Aux[A, HList.FromTuple[m.MirroredElemTypes]] =
      new Inst[A] {
        type Repr = HList.FromTuple[m.MirroredElemTypes]
        def to(a: A): Repr = HList.fromTuple(Tuple.fromProductTyped(a))
        def from(r: Repr): A = m.fromTuple(HList.toTuple(r).asInstanceOf[m.MirroredElemTypes])
      }
  }
}
