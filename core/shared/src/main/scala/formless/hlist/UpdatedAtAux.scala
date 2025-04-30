package formless.hlist

final class UpdatedAtAux[L, N](private val l: L) extends AnyVal {
  final def apply[U, V, O <: HList](u: U)(implicit r: ReplaceAt[L, N, U] { type Out <: (V, O) }): O = r(l, u)._2
}
