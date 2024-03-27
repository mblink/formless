package formless.hlist

trait Tupler[L] extends DepFn1[L] with Serializable

object Tupler {
  type Aux[L, O] = Tupler[L] { type Out = O }

  inline def apply[L](using t: Tupler[L]): Tupler.Aux[L, t.Out] = t

  given tuplerHList[L <: HList]: Tupler.Aux[L, HList.ToTuple[L]] =
    new Tupler[L] {
      type Out = HList.ToTuple[L]
      def apply(l: L): Out = HList.toTuple(l)
    }
}
