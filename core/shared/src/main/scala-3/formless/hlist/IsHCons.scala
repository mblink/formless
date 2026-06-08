package formless.hlist

/**
 * Type class witnessing that this `HList` is composite and providing access to head and tail.
 */
trait IsHCons[L <: HList] extends Serializable {
  type H
  type T <: HList

  def head(l: L): H
  def tail(l: L): T
  def cons(h: H, t: T): L
}

object IsHCons {
  def apply[L <: HList](using n: IsHCons[L]): IsHCons.Aux[L, n.H, n.T] = n

  type Aux[L <: HList, H0, T0 <: HList] = IsHCons[L] { type H = H0; type T = T0 }

  given isHConsInst[H0, T0 <: HList]: IsHCons.Aux[H0 :: T0, H0, T0] =
    new IsHCons[H0 :: T0] {
      type H = H0
      type T = T0

      def head(l: H0 :: T0): H = l.head
      def tail(l: H0 :: T0): T = l.tail
      def cons(h: H0, t: T0): H0 :: T0 = h :: t
    }
}
