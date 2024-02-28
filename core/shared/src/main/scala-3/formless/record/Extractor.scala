package formless.record

import formless.hlist.{::, HList, HNil}
import scala.util.NotGiven

/**
 * Type class supporting extraction of super-record from sub-record (witnesses depth subtype relation).
 */
trait Extractor[L, E] extends (L => E) with Serializable

trait ExtractorLP {
  given extract[L <: HList, K, V, ET <: HList, V1, LR <: HList](
    using ev0: NotGiven[L =:= ((K ->> V) :: ET)],
    r: Remover.Aux[L, K, (V1, LR)],
    ev: V1 <:< V,
    ds: Extractor[LR, ET]
  ): Extractor[L, (K ->> V) :: ET] =
  new Extractor[L, (K ->> V) :: ET] {
    def apply(c: L): (K ->> V) :: ET = {
      val (h, t) = r(c)
      label[K](ev(h)) :: ds(t)
    }
  }
}

object Extractor extends ExtractorLP {
  inline def apply[L, E](using e: Extractor[L, E]): Extractor[L, E] = e

  given hnil[L, E](using ev: HNil =:= E): Extractor[L, E] =
    new Extractor[L, E] {
      def apply(c: L): E = HNil
    }

  private val identicalInst: Extractor[HList, HList] = t => t

  given identical[L <: HList]: Extractor[L, L] = identicalInst.asInstanceOf[Extractor[L, L]]

  given descend[L <: HList, K, V <: HList, V1 <: HList, LR <: HList, ET <: HList](
    using ev0: NotGiven[L =:= ((K ->> V) :: ET)],
    r: Remover.Aux[L, K, (V1, LR)],
    ds1: Extractor[V1, V],
    ds2: Extractor[LR, ET]
  ): Extractor[L, (K ->> V) :: ET] =
    new Extractor[L, (K ->> V) :: ET] {
      def apply(c: L): (K ->> V) :: ET = {
        val (h, t) = r(c)
        label[K](ds1(h)) :: ds2(t)
      }
    }
}
