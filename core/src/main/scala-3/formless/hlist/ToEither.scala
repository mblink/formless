package formless.hlist

type ToEitherT[T <: HList] = T match {
  case HNil => Nothing
  case l :: HNil => Either[l, Nothing]
  case l :: r :: HNil => Either[l, r]
  case l :: r => Either[l, ToEitherT[r]]
}

/**
 * Type class computing the `Either` type corresponding to this `HList`.
 */
trait ToEither[T] extends Serializable {
  type Out
}

object ToEither {
  type Aux[T, O] = ToEither[T] { type Out = O }

  inline def apply[T <: HList](using e: ToEither[T]): ToEither.Aux[T, e.Out] = e

  given toEitherHList[T <: HList]: ToEither.Aux[T, ToEitherT[T]] =
    new ToEither[T] {
      type Out = ToEitherT[T]
    }
}
