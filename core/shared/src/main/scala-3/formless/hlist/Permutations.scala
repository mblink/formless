package formless.hlist

/**
 * Type class supporting the calculation of every permutation of this `HList`
 */
trait Permutations[L] extends DepFn1[L], Serializable

object Permutations {
  type Aux[L, O] = Permutations[L] { type Out = O }

  inline def apply[L](using p: Permutations[L]): Permutations.Aux[L, p.Out] = p

  given permutationsHNil[L <: HNil]: Permutations.Aux[L, HNil :: HNil] =
    new Permutations[L] {
      type Out = HNil :: HNil
      def apply(l: L): Out = HNil :: HNil
    }

  given permutationsHCons[H, T <: HList, TP <: HList, FlatmapInterleaveOut <: HList](
    using pt: Permutations.Aux[T, TP],
    ih: FlatMapInterleave.Aux[H, TP, FlatmapInterleaveOut],
  ): Permutations.Aux[H :: T, FlatmapInterleaveOut] =
    new Permutations[H :: T] {
      type Out = FlatmapInterleaveOut
      def apply(l: H :: T): Out = ih(l.head, pt(l.tail))
    }
}
