package formless.hlist

/**
 * Type class witnessing that the result of stripping type constructor `F` off each element of `HList` `L` is `Out`.
 */
trait Comapped[T, F[_]] extends Serializable {
  type Out
}

object Comapped {
  type Aux[T, F[_], O] = Comapped[T, F] { type Out = O }

  inline def apply[T <: HList, F[_]](using m: Comapped[T, F]): Comapped.Aux[T, F, m.Out] = m

  given comappedHList[T <: HList, F[_]](using ev: HList.IsMappedBy[F][T]): Comapped.Aux[T, F, HList.InverseMap[T, F]] =
    new Comapped[T, F] {
      type Out = HList.InverseMap[T, F]
    }
}
