package typify.tuple

trait ReverseSplitRight[L, U] extends DepFn1[L] {
  type Prefix
  type Suffix
  final type Out = (Prefix, Suffix)

  def product(l: L): Prefix *: Suffix *: EmptyTuple

  final def apply(l: L): Out = {
    val p = product(l)
    (p.head, p.tail.head)
  }
}

object ReverseSplitRight {
  type Aux[L, U, P, S] = ReverseSplitRight[L, U] {
    type Prefix = P
    type Suffix = S
  }

  inline def apply[T, U](using s: ReverseSplitRight[T, U]): ReverseSplitRight.Aux[T, U, s.Prefix, s.Suffix] = s

  given reverseSplitRightTuple[L <: Tuple, U](
    using idxv: ValueOf[ElemIndex[ReverseT[L], U]],
  ): ReverseSplitRight.Aux[L, U, Tuple.Drop[ReverseT[L], ElemIndex[ReverseT[L], U]], ReverseT[Tuple.Take[ReverseT[L], ElemIndex[ReverseT[L], U]]]] =
    new ReverseSplitRight[L, U] {
      type Prefix = Tuple.Drop[ReverseT[L], ElemIndex[ReverseT[L], U]]
      type Suffix = ReverseT[Tuple.Take[ReverseT[L], ElemIndex[ReverseT[L], U]]]

      private lazy val n = idxv.value
      def product(l: L): Prefix *: Suffix *: EmptyTuple = {
        val a = l.toArray.reverse
        Tuple.fromArray(a.drop(n)).asInstanceOf[Prefix] *:
          Tuple.fromArray(a.take(n).reverse).asInstanceOf[Suffix] *:
          EmptyTuple
      }
    }
}