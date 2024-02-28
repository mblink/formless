package formless.hlist

trait ToEither[T] extends Serializable {
  type Out
}

sealed trait ToEitherLP {
  final type Aux[T, O] = ToEither[T] { type Out = O }

  final implicit def toEitherHCons[L, R <: HList](implicit r: ToEither[R]): ToEither.Aux[L :: R, Either[L, r.Out]] =
    new ToEither[L :: R] {
      type Out = Either[L, r.Out]
    }
}

object ToEither extends ToEitherLP {
  def apply[T <: HList](implicit e: ToEither[T]): ToEither.Aux[T, e.Out] = e

  implicit val toEitherHNil: ToEither.Aux[HNil, Nothing] =
    new ToEither[HNil] {
      type Out = Nothing
    }

  implicit def toEitherHNil1[L]: ToEither.Aux[L :: HNil, Either[L, Nothing]] =
    new ToEither[L :: HNil] {
      type Out = Either[L, Nothing]
    }

  implicit def toEitherHNil2[L, R]: ToEither.Aux[L :: R :: HNil, Either[L, R]] =
    new ToEither[L :: R :: HNil] {
      type Out = Either[L, R]
    }
}
