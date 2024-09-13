package formless.hlist

/**
 * Type class supporting access to the elements of this `HList` specified by `Ids`. Available only if this `HList`
 * contains all elements specified in `Ids`.
 */
trait SelectMany[L, Ids] extends DepFn1[L], Serializable

object SelectMany {
  type Aux[L, Ids, O] = SelectMany[L, Ids] { type Out = O }

  inline def apply[L, Ids](using s: SelectMany[L, Ids]): SelectMany.Aux[L, Ids, s.Out] = s

  given selectManyHNil[L]: SelectMany.Aux[L, HNil, HNil] =
    new SelectMany[L, HNil] {
      type Out = HNil
      def apply(l: L): Out = HNil
    }

  given selectManyHCons[L <: HList, H, T <: HList, SelOut <: HList, AtOut](
    using s: SelectMany.Aux[L, T, SelOut],
    a: At.Aux[L, H, AtOut],
  ): SelectMany.Aux[L, H :: T, AtOut :: SelOut] =
    new SelectMany[L, H :: T] {
      type Out = AtOut :: SelOut
      def apply(l: L): Out = a(l) :: s(l)
    }
}
