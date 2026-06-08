package formless
package hlist

/**
 * Type class supporting zipping a `HList` with its element indices, resulting in a `HList` of `HList`s of the form
 * ({element from input `HList`}, {element index})
 */
trait ZipWithIndex[L] extends DepFn1[L] with Serializable

object ZipWithIndex {
  type Aux[L, O] = ZipWithIndex[L] { type Out = O }

  def apply[L](implicit z: ZipWithIndex[L]): ZipWithIndex.Aux[L, z.Out] = z

  object natToInt extends Poly1 {
    implicit def inst[A, N <: shapeless.Nat, I <: Int](implicit ni: NatToInt.Aux[N, I]): Case.Aux[(A, N), (A, I)] =
      at { case (a, _) => (a, ni()) }
  }

  implicit def zipWithIndexHList[L <: HList, O <: HList, MO <: HList](
    implicit sz: shapeless.ops.hlist.ZipWithIndex.Aux[L, O],
    m: Mapper.Aux[natToInt.type, O, MO],
  ): ZipWithIndex.Aux[L, MO] =
    new ZipWithIndex[L] {
      type Out = MO
      def apply(l: L): Out = m(sz(l))
    }
}
