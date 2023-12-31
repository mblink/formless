package formless.tuple

import scala.compiletime.summonAll

/**
 * Type class supporting producing a `Tuple` filled from a `Poly` of type `F`.
 */
trait FillWith[F, L] extends DepFn0 with Serializable { final type Out = L }

object FillWith {
  inline def apply[F, L](using f: FillWith[F, L]): FillWith[F, L] = f

  inline given fillWithInst[F <: Poly, L <: Tuple]: FillWith[F, L] =
    new FillWith[F, L] {
      def apply(): L =
        Tuple.fromArray(summonAll[Tuple.Map[L, Case0.Aux[F, *]]]
          .toArray.map(_.asInstanceOf[Case0.Aux[F, Any]].apply())).asInstanceOf[L]
    }
}
