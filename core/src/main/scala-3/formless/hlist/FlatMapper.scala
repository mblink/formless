package formless.hlist

/**
 * Type class supporting flatmapping a higher ranked function of type `F` over this `HList`.
 */
trait FlatMapper[F, In <: HList] extends DepFn1[In], Serializable { type Out <: HList }

object FlatMapper {
  type Aux[F, In <: HList, O <: HList] = FlatMapper[F, In] { type Out = O }

  inline def apply[F, L <: HList](using m: FlatMapper[F, L]): FlatMapper.Aux[F, L, m.Out] = m

  given flatMapperHNil[F]: FlatMapper.Aux[F, HNil, HNil] =
    new FlatMapper[F, HNil] {
      type Out = HNil
      def apply(t: HNil): Out = HNil
    }

  given flatMapperHCons[F <: Poly, InH, InT <: HList, OutH <: HList, OutT <: HList, Out0 <: HList](
    using hc: Case1.Aux[F, InH, OutH],
    mt: FlatMapper.Aux[F, InT, OutT],
    p: Prepend.Aux[OutH, OutT, Out0],
  ): FlatMapper.Aux[F, InH :: InT, Out0] =
    new FlatMapper[F, InH :: InT] {
      type Out = Out0
      def apply(t: InH :: InT): Out = p(hc(t.head), mt(t.tail))
    }
}
