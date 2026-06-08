package formless.hlist

final class PatchAux[L, N, M](private val l: L) extends AnyVal {
  final def apply[In <: HList](in: In)(implicit p: Patcher[N, M, L, In]): p.Out = p(l, in)
}
