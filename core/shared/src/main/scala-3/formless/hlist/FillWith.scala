package formless.hlist

/**
 * Type class supporting producing a `HList` filled from a `Poly` of type `F`.
 */
trait FillWith[F, L] extends DepFn0, Serializable { final type Out = L }

object FillWith {
  inline def apply[F, L](using f: FillWith[F, L]): FillWith[F, L] = f

  final class Inst[F, L](l: L) extends FillWith[F, L], Serializable {
    final def apply(): L = l
  }

  inline given fillWithInst[F <: Poly, L <: HList]: FillWith[F, L] =
    Inst(
      HList.fromArray(summonAllHList[HList.Map[L, Case0.Aux[F, *]]]
        .toArray.map(_.asInstanceOf[Case0.Aux[F, Any]].apply())).asInstanceOf[L]
    )
}
