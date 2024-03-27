package formless.hlist

/**
 * Type class supporting permuting this `HList` into the same order as another `HList` with the same element types.
 */
trait Align[L, M] extends (L => M) with Serializable

object Align {
  inline def apply[L, M](using a: Align[L, M]): Align[L, M] = a

  given alignHNil: Align[HNil, HNil] =
    new Align[HNil, HNil] {
      def apply(l: HNil): HNil = l
    }

  given alignHCons[L <: HList, MH, MT <: HList, R](
    using r: Remove.Aux[L, MH, (MH, R)],
    at: Align[R, MT],
  ): Align[L, MH :: MT] =
    new Align[L, MH :: MT] {
      def apply(l: L): MH :: MT = {
        val (h, t) = r(l)
        h :: at(t)
      }
    }
}
