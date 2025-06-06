package formless.hlist

import compiletime.ops.int.>=

type GrouperT[L <: HList, N <: Int, Step <: Int] <: HList =
  (HList.Size[L] >= N) match {
    case true => HList.ToTuple[HList.Take[L, N]] :: GrouperT[HList.Drop[L, Step], N, Step]
    case false => HNil
  }

/**
 * Typeclass supporting grouping this `HList` into `HList`s of `N` items each, at `Step` apart.
 * If `Step` equals `N` then the groups do not overlap.
 */
trait Grouper[L, N, Step] extends DepFn1[L], Serializable

object Grouper {
  type Aux[L, N, Step, O] = Grouper[L, N, Step] { type Out = O }

  inline def apply[L, N, Step](using g: Grouper[L, N, Step]): Grouper.Aux[L, N, Step, g.Out] = g

  given grouperHList[L <: HList, N <: Int, Step <: Int](
    using nv: ValueOf[N],
    stepv: ValueOf[Step],
  ): Grouper.Aux[L, N, Step, GrouperT[L, N, Step]] =
    new Grouper[L, N, Step] {
      type Out = GrouperT[L, N, Step]

      private lazy val n = nv.value
      private lazy val step = stepv.value

      @annotation.tailrec
      private def go(a: Array[Any], acc: Array[Tuple]): Array[Tuple] =
        if (a.sizeIs >= n) go(a.drop(step), acc :+ Tuple.fromArray(a.take(n)))
        else acc

      def apply(l: L): Out = HList.fromArray(go(l.toArray, Array())).asInstanceOf[Out]
    }
}
