package formless.hlist

import compiletime.ops.int.{-, >=}

type RepeatT[L <: HList, N <: Int] = N match {
  case 0 => HNil
  case _ => HList.Concat[RepeatT[L, N - 1], L]
}

/**
 * Typeclass supporting repeating a `HList` of type `L` `N` times.
 */
trait Repeat[L, N] extends DepFn1[L], Serializable

object Repeat {
  type Aux[L, N, O] = Repeat[L, N] { type Out = O }

  inline def apply[L, N](using r: Repeat[L, N]): Repeat.Aux[L, N, r.Out] = r

  given repeatHList[L <: HList, N <: Int](
    using ev: (N >= 1) =:= true,
    n: ValueOf[N]
  ): Repeat.Aux[L, N, RepeatT[L, N]] =
    new Repeat[L, N] {
      type Out = RepeatT[L, N]
      def apply(l: L): Out = {
        var i: Int = n.value
        if (i == 0) HNil.asInstanceOf[Out]
        else {
          val b = Array.newBuilder[Any]
          val a = l.toArray
          while (i > 0) {
            b ++= a
            i -= 1
          }
          HList.fromArray(b.result()).asInstanceOf[Out]
        }
      }
    }
}
