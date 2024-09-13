package formless.hlist

/**
 * Type class supporting mapping a higher ranked function over this `HList`.
 */
trait Mapper[F, In <: HList] extends DepFn1[In], Serializable { type Out <: HList }

object Mapper {
  type Aux[F, In <: HList, O <: HList] = Mapper[F, In] { type Out = O }

  inline def apply[F, L <: HList](using m: Mapper[F, L]): Mapper.Aux[F, L, m.Out] = m

  given mapperHNil[F]: Mapper.Aux[F, HNil, HNil] =
    new Mapper[F, HNil] {
      type Out = HNil
      def apply(l: HNil): Out = HNil
    }

  given mapperHCons[F <: Poly, InH, InT <: HList, OutH, OutT <: HList](
    using hc: Case1.Aux[F, InH, OutH],
    mt: Mapper.Aux[F, InT, OutT],
  ): Mapper.Aux[F, InH :: InT, OutH :: OutT] =
    new Mapper[F, InH :: InT] {
      type Out = OutH :: OutT
      def apply(l: InH :: InT): Out = hc(l.head) :: mt(l.tail)
    }
}
