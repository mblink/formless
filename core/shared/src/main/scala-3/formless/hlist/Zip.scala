package formless.hlist

/**
 * Type class supporting zipping this `HList` of monomorphic function values with its argument `HList` of
 * correspondingly typed function arguments returning the result of each application as a `HList`. Available only if
 * there is evidence that the corresponding function and argument elements have compatible types.
 */
trait Zip[L] extends DepFn1[L] with Serializable { type Out <: HList }

object Zip {
  type Aux[L, O] = Zip[L] { type Out = O }

  inline def apply[L](using z: Zip[L]): Zip.Aux[L, z.Out] = z

  given zipHList[L <: HList, OutT <: HList, OutM <: HList](
    using t: Transposer.Aux[L, OutT],
    m: Mapper.Aux[tupled.type, OutT, OutM],
  ): Zip.Aux[L, OutM] =
    new Zip[L] {
      type Out = OutM
      def apply(l : L): Out = l.transpose.map(tupled)
    }
}
