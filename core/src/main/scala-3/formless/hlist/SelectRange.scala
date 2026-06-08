package formless.hlist

import compiletime.ops.int.{-, >=}

/**
 * Type class supporting supporting access to the elements in range [a,b] of this `HList`.
 * Available only if this `HList` contains all elements in range.
 */
trait SelectRange[L, A, B] extends DepFn1[L], Serializable

object SelectRange {
  type Aux[L, A, B, O] = SelectRange[L, A, B] { type Out = O }

  inline def apply[L, A, B](using s: SelectRange[L, A, B]): SelectRange.Aux[L, A, B, s.Out] = s

  given selectRangeHList[L <: HList, A <: Int, B <: Int](
    using av: ValueOf[A],
    bv: ValueOf[B],
    evA: (A >= 0) =:= true,
    evB: (B >= A) =:= true,
  ): SelectRange.Aux[L, A, B, HList.Take[HList.Drop[L, A], B - A]] =
    new SelectRange[L, A, B] {
      type Out = HList.Take[HList.Drop[L, A], B - A]
      private lazy val a = av.value
      private lazy val b = bv.value
      def apply(l: L): Out = HList.fromArray(l.toArray.drop(a).take(b - a)).asInstanceOf[Out]
    }
}
