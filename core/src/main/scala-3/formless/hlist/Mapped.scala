package formless.hlist

/**
 * Type class witnessing that the result of wrapping each element of `HList` `L` in type constructor `F` is `Out`.
 */
trait Mapped[T, F[_]] extends Serializable {
  type Out
}

object Mapped {
  type Aux[T, F[_], O] = Mapped[T, F] { type Out = O }

  inline def apply[T <: HList, F[_]](using m: Mapped[T, F]): Mapped.Aux[T, F, m.Out] = m

  given mappedHList[T <: HList, F[_]]: Mapped.Aux[T, F, HList.Map[T, F]] =
    new Mapped[T, F] {
      type Out = HList.Map[T, F]
    }
}
