package formless.hlist

/**
 * Type class supporting unzipping this `HList` of tuples returning a tuple of `HList`s.
 */
trait Unzip[L] extends DepFn1[L] with Serializable

object Unzip {
  type Aux[L, O] = Unzip[L] { type Out = O }

  inline def apply[L](using u: Unzip[L]): Unzip.Aux[L, u.Out] = u

  given unzipper[L <: HList, OutM <: HList, TransposerOut <: HList, TuplerOut](
    using m: Mapper.Aux[productElements.type, L, OutM],
    tr: Transposer.Aux[OutM, TransposerOut],
    tu: Tupler.Aux[TransposerOut, TuplerOut],
  ): Unzip.Aux[L, TuplerOut] =
    new Unzip[L] {
      type Out = TuplerOut
      def apply(l : L): Out = tu(tr(m(l)))
    }
}
