package formless.hlist

import scala.language.implicitConversions

final class HListOps[L <: HList](private val l: L) extends AnyVal {
  /**
   * Returns the `N`th element of this `HList`. An explicit type argument must be provided. Available only if there is
   * evidence that this `HList` has at least `N` elements.
   */
  final def at[N <: Int](implicit a: At[L, N]): a.Out = a(l)

  /**
   * Returns the `N`th element of this `HList`. Available only if there is evidence that this `HList` has at least `N`
   * elements.
   */
  final def at(n: Int)(implicit a: At[L, n.type]): a.Out = a(l)

  /**
   * Compute the length of this `HList`.
   */
  final def length(implicit len: Length[L]): len.Out = len()

  /**
    * Repeats this `HList` N times.
    */
  final def repeat[N <: Int](implicit r: Repeat[L, N]): r.Out = r(l)

  /**
   * Rotate this `HList` left by N. An explicit type argument must be provided.
   */
  final def rotateLeft[N](implicit r: RotateLeft[L, N]): r.Out = r(l)

  /**
   * Rotate this `HList` left by N
   */
  final def rotateLeft(n: Int)(implicit r: RotateLeft[L, n.type]): r.Out = r(l)

  /**
   * Rotate this `HList` right by N. An explicit type argument must be provided.
   */
  final def rotateRight[N](implicit r: RotateRight[L, N]): r.Out = r(l)

  /**
   * Rotate this `HList` right by N
   */
  final def rotateRight(n: Int)(implicit r: RotateRight[L, n.type]): r.Out = r(l)

  /**
   * Converts this `HList` to a `M` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  final def toLub[M[_], Lb](implicit t: ToTraversable.Aux[L, M, Lb]): t.Out = t(l)

  final def selectManyType[Ids <: HList](implicit s: SelectMany[L, Ids]): s.Out = s(l)

  /**
   * Returns the elements of this `HList` specified by the range of ids in [A,B[
   * Available only if there is evidence that this `HList` contains all elements in that range
   */
  final def selectRange[A <: Int, B <: Int](implicit s: SelectRange[L,A,B]): s.Out = s(l)

  final def selectRange(a: Int, b: Int)(implicit s: SelectRange[L, a.type, b.type]): s.Out = s(l)

  /**
   * Replaces the `N`th element of this `HList` with the result of calling the supplied function on it.
   * Available only if there is evidence that this `HList` has `N` elements.
   *
   * @author Andreas Koestler
   */
  def updateAtWith[V](n: At.WithInt[L])(f: n.O => V)(implicit m: ModifierAt[L, n.N, n.O, V]): m.Out = m(l, f)

  /**
   * Replaces the ''nth' element of this `HList` with the supplied value of type `U`. An explicit type argument
   * must be provided for `N`. Available only if there is evidence that this `HList` has at least ''n'' elements.
   */
  final def updatedAt[N]: UpdatedAtAux[L, N] = new UpdatedAtAux[L, N](l)

  /**
   * Replaces the `n`th element of this `HList` with the supplied value of type `U`. Available only if there is
   * evidence that this `HList` has at least `n` elements.
   */
  final def updatedAt[U, V, O <: HList](n: Int, u: U)(implicit r: ReplaceAt[L, n.type, U] { type Out <: (V, O) }): O = r(l, u)._2

  /**
   * Returns the first `N` elements of this `HList`. An explicit type argument must be provided. Available only if
   * there is evidence that this `HList` has at least `N` elements.
   */
  final def take[N](implicit t: Take[L, N]): t.Out = t(l)

  /**
   * Returns the first `n` elements of this `HList`. Available only if there is evidence that this `HList` has at
   * least `n` elements.
   */
  final def take(n: Int)(implicit t: Take[L, n.type]): t.Out = t(l)

  /**
   * Returns all but the  first `N` elements of this `HList`. An explicit type argument must be provided. Available
   * only if there is evidence that this `HList` has at least `N` elements.
   */
  final def drop[N](implicit d: Drop[L, N]): d.Out = d(l)

  /**
   * Returns all but the  first `n` elements of this `HList`. Available only if there is evidence that this `HList`
   * has at least `n` elements.
   */
  final def drop(n: Int)(implicit d: Drop[L, n.type]): d.Out = d(l)

  /**
   * Splits this `HList` at the `N`th element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has at least `N` elements.
   */
  final def split[N](implicit s: Split[L, N]): s.Out = s(l)

  /**
   * Splits this `HList` at the `n`th element, returning the prefix and suffix as a pair. Available only if there is
   * evidence that this `HList` has at least `n` elements.
   */
  final def split(n: Int)(implicit s: Split[L, n.type]): s.Out = s(l)

  /**
   * Splits this `HList` at the `N`th element, returning the reverse of the prefix and suffix as a pair. An explicit
   * type argument must be provided. Available only if there is evidence that this `HList` has at least `N` elements.
   */
  final def reverse_split[N](implicit s: ReverseSplit[L, N]): s.Out = s(l)

  /**
   * Splits this `HList` at the `n`th element, returning the reverse of the prefix and suffix as a pair. Available
   * only if there is evidence that this `HList` has at least `n` elements.
   */
  final def reverse_split(n: Int)(implicit s: ReverseSplit[L, n.type]): s.Out = s(l)

  /**
   * Zips this `HList` with its element indices,  resulting in a `HList` of  `HList`s of the form
   * ({element from input `HList`}, {element index})
   */
  final def zipWithIndex(implicit z: ZipWithIndex[L]): z.Out = z(l)

  /**
   *
   * Produces a new `HList` where a slice of this `HList` is replaced by another. Available only if there are at least
   * ``n`` plus ``m`` elements.
   */
  final def patch[In <: HList](n: Int, in: In, m: Int)(implicit p: Patcher[n.type, m.type, L, In]): p.Out = p(l, in)

  /**
   * Produces a new `HList` where a slice of this `HList` is replaced by another. Two explicit type arguments must be
   * provided. Available only if there are at least `N` plus `M` elements.
   */
  final def patch[N, M]: PatchAux[L, N, M] = new PatchAux[L, N, M](l)

  /**
   * Groups the elements of this `HList` into `HList`s of `n` elements, offset by `step`
   */
  final def group(n: Int, step: Int)(implicit g: Grouper[L, n.type, step.type]): g.Out = g(l)

  /**
   * Returns all combinations of exactly length `N` of elements from this `HList`
   */
  final def combinations(n: Int)(implicit c: Combinations[n.type, L]): c.Out = c(l)
}

trait HListPackageCompat {
  final type HList = shapeless.HList
  final val HList: shapeless.HList.type = shapeless.HList
  final type ::[H, T <: HList] = shapeless.::[H, T]
  final val :: : shapeless.::.type = shapeless.::
  final type HNil = shapeless.HNil
  final val HNil: HNil = shapeless.HNil

  final implicit def toHListOps[L <: HList](l: L): HListOps[L] = new HListOps[L](l)

  final type Align[L <: HList, M <: HList] = shapeless.ops.hlist.Align[L, M]
  final val Align: shapeless.ops.hlist.Align.type = shapeless.ops.hlist.Align

  final type Collect[I <: HList, P <: shapeless.Poly] = shapeless.ops.hlist.Collect[I, P]
  final val Collect: shapeless.ops.hlist.Collect.type = shapeless.ops.hlist.Collect

  final type CollectFirst[L <: HList, P <: shapeless.Poly] = shapeless.ops.hlist.CollectFirst[L, P]
  final val CollectFirst: shapeless.ops.hlist.CollectFirst.type = shapeless.ops.hlist.CollectFirst

  final type Comapped[T <: HList, F[_]] = shapeless.ops.hlist.Comapped[T, F]
  final val Comapped: shapeless.ops.hlist.Comapped.type = shapeless.ops.hlist.Comapped

  final type ConstMapper[C, T <: HList] = shapeless.ops.hlist.ConstMapper[C, T]
  final val ConstMapper: shapeless.ops.hlist.ConstMapper.type = shapeless.ops.hlist.ConstMapper

  final type Diff[L <: HList, M <: HList] = shapeless.ops.hlist.Diff[L, M]
  final val Diff: shapeless.ops.hlist.Diff.type = shapeless.ops.hlist.Diff

  final type Generic[A] = shapeless.Generic[A]
  final val Generic: shapeless.Generic.type = shapeless.Generic

  final type FillWith[F, L <: HList] = shapeless.ops.hlist.FillWith[F, L]
  final val FillWith: shapeless.ops.hlist.FillWith.type = shapeless.ops.hlist.FillWith

  final type Filter[L <: HList, U] = shapeless.ops.hlist.Filter[L, U]
  final val Filter: shapeless.ops.hlist.Filter.type = shapeless.ops.hlist.Filter

  final type FilterNot[L <: HList, U] = shapeless.ops.hlist.FilterNot[L, U]
  final val FilterNot: shapeless.ops.hlist.FilterNot.type = shapeless.ops.hlist.FilterNot

  final type FlatMapper[F, In <: HList] = shapeless.ops.hlist.FlatMapper[F, In]
  final val FlatMapper: shapeless.ops.hlist.FlatMapper.type = shapeless.ops.hlist.FlatMapper

  final type FlatMapInterleave[A, L <: HList] = shapeless.ops.hlist.FlatMapInterleave[A, L]
  final val FlatMapInterleave: shapeless.ops.hlist.FlatMapInterleave.type = shapeless.ops.hlist.FlatMapInterleave

  final type Init[T <: HList] = shapeless.ops.hlist.Init[T]
  final val Init: shapeless.ops.hlist.Init.type = shapeless.ops.hlist.Init

  final type Interleave[A, L <: HList] = shapeless.ops.hlist.Interleave[A, L]
  final val Interleave: shapeless.ops.hlist.Interleave.type = shapeless.ops.hlist.Interleave

  final type Intersection[L <: HList, M <: HList] = shapeless.ops.hlist.Intersection[L, M]
  final val Intersection: shapeless.ops.hlist.Intersection.type = shapeless.ops.hlist.Intersection

  final type IsHCons[T <: HList] = shapeless.ops.hlist.IsHCons[T]
  final val IsHCons: shapeless.ops.hlist.IsHCons.type = shapeless.ops.hlist.IsHCons

  final type Last[T <: HList] = shapeless.ops.hlist.Last[T]
  final val Last: shapeless.ops.hlist.Last.type = shapeless.ops.hlist.Last

  final type LeftFolder[L <: HList, In, F] = shapeless.ops.hlist.LeftFolder[L, In, F]
  final val LeftFolder: shapeless.ops.hlist.LeftFolder.type = shapeless.ops.hlist.LeftFolder

  final type LeftReducer[L <: HList, F] = shapeless.ops.hlist.LeftReducer[L, F]
  final val LeftReducer: shapeless.ops.hlist.LeftReducer.type = shapeless.ops.hlist.LeftReducer

  final type LeftScanner[L <: HList, In, P <: shapeless.Poly] = shapeless.ops.hlist.LeftScanner[L, In, P]
  final val LeftScanner: shapeless.ops.hlist.LeftScanner.type = shapeless.ops.hlist.LeftScanner

  final type LiftAll[F[_], In <: HList] = shapeless.ops.hlist.LiftAll[F, In]
  final val LiftAll: shapeless.ops.hlist.LiftAll.type = shapeless.ops.hlist.LiftAll

  final type Lub[-A, -B, Out] = shapeless.Lub[A, B, Out]
  final val Lub: shapeless.Lub.type = shapeless.Lub

  final type MapCons[A, M <: HList] = shapeless.ops.hlist.MapCons[A, M]
  final val MapCons: shapeless.ops.hlist.MapCons.type = shapeless.ops.hlist.MapCons

  final type MapFolder[T <: HList, R, F] = shapeless.ops.hlist.MapFolder[T, R, F]
  final val MapFolder: shapeless.ops.hlist.MapFolder.type = shapeless.ops.hlist.MapFolder

  final type Mapped[T <: HList, F[_]] = shapeless.ops.hlist.Mapped[T, F]
  final val Mapped: shapeless.ops.hlist.Mapped.type = shapeless.ops.hlist.Mapped

  final type Mapper[F, In <: HList] = shapeless.ops.hlist.Mapper[F, In]
  final val Mapper: shapeless.ops.hlist.Mapper.type = shapeless.ops.hlist.Mapper

  final type Modifier[L <: HList, U, V] = shapeless.ops.hlist.Modifier[L, U, V]
  final val Modifier: shapeless.ops.hlist.Modifier.type = shapeless.ops.hlist.Modifier

  final type NotContains[L <: HList, U] = shapeless.NotContainsConstraint[L, U]
  final val NotContains: shapeless.NotContainsConstraint.type = shapeless.NotContainsConstraint

  final type PadTo[N, A, L <: HList] = shapeless.ops.hlist.PadTo[N, A, L]
  final val PadTo: shapeless.ops.hlist.PadTo.type = shapeless.ops.hlist.PadTo

  final type Partition[L <: HList, U] = shapeless.ops.hlist.Partition[L, U]
  final val Partition: shapeless.ops.hlist.Partition.type = shapeless.ops.hlist.Partition

  final type Permutations[T <: HList] = shapeless.ops.hlist.Permutations[T]
  final val Permutations: shapeless.ops.hlist.Permutations.type = shapeless.ops.hlist.Permutations

  final type Prepend[L <: HList, R <: HList] = shapeless.ops.hlist.Prepend[L, R]
  final val Prepend: shapeless.ops.hlist.Prepend.type = shapeless.ops.hlist.Prepend

  final type Reify[T <: HList] = shapeless.ops.hlist.Reify[T]
  final val Reify: shapeless.ops.hlist.Reify.type = shapeless.ops.hlist.Reify

  final type Remove[L <: HList, U] = shapeless.ops.hlist.Remove[L, U]
  final val Remove: shapeless.ops.hlist.Remove.type = shapeless.ops.hlist.Remove

  final type RemoveAll[L <: HList, SL <: HList] = shapeless.ops.hlist.RemoveAll[L, SL]
  final val RemoveAll: shapeless.ops.hlist.RemoveAll.type = shapeless.ops.hlist.RemoveAll

  final type Replacer[L <: HList, U, V] = shapeless.ops.hlist.Replacer[L, U, V]
  final val Replacer: shapeless.ops.hlist.Replacer.type = shapeless.ops.hlist.Replacer

  final type Reverse[T <: HList] = shapeless.ops.hlist.Reverse[T]
  final val Reverse: shapeless.ops.hlist.Reverse.type = shapeless.ops.hlist.Reverse

  final type ReversePrepend[L <: HList, R <: HList] = shapeless.ops.hlist.ReversePrepend[L, R]
  final val ReversePrepend: shapeless.ops.hlist.ReversePrepend.type = shapeless.ops.hlist.ReversePrepend

  final type ReverseSplitLeft[T <: HList, U] = shapeless.ops.hlist.ReverseSplitLeft[T, U]
  final val ReverseSplitLeft: shapeless.ops.hlist.ReverseSplitLeft.type = shapeless.ops.hlist.ReverseSplitLeft

  final type ReverseSplitRight[T <: HList, U] = shapeless.ops.hlist.ReverseSplitRight[T, U]
  final val ReverseSplitRight: shapeless.ops.hlist.ReverseSplitRight.type = shapeless.ops.hlist.ReverseSplitRight

  final type RightFolder[L <: HList, In, F] = shapeless.ops.hlist.RightFolder[L, In, F]
  final val RightFolder: shapeless.ops.hlist.RightFolder.type = shapeless.ops.hlist.RightFolder

  final type RightReducer[L <: HList, F] = shapeless.ops.hlist.RightReducer[L, F]
  final val RightReducer: shapeless.ops.hlist.RightReducer.type = shapeless.ops.hlist.RightReducer

  final type RightScanner[L <: HList, In, P <: shapeless.Poly] = shapeless.ops.hlist.RightScanner[L, In, P]
  final val RightScanner: shapeless.ops.hlist.RightScanner.type = shapeless.ops.hlist.RightScanner

  final type Selector[T <: HList, A] = shapeless.ops.hlist.Selector[T, A]
  final val Selector: shapeless.ops.hlist.Selector.type = shapeless.ops.hlist.Selector

  final type SelectAll[L <: HList, S <: HList] = shapeless.ops.hlist.SelectAll[L, S]
  final val SelectAll: shapeless.ops.hlist.SelectAll.type = shapeless.ops.hlist.SelectAll

  final type Slice[N, U, L <: HList] = shapeless.ops.hlist.Slice[N, U, L]
  final val Slice: shapeless.ops.hlist.Slice.type = shapeless.ops.hlist.Slice

  final type SplitLeft[T <: HList, U] = shapeless.ops.hlist.SplitLeft[T, U]
  final val SplitLeft: shapeless.ops.hlist.SplitLeft.type = shapeless.ops.hlist.SplitLeft

  final type SplitRight[T <: HList, U] = shapeless.ops.hlist.SplitRight[T, U]
  final val SplitRight: shapeless.ops.hlist.SplitRight.type = shapeless.ops.hlist.SplitRight

  final type SubtypeUnifier[T <: HList, B] = shapeless.ops.hlist.SubtypeUnifier[T, B]
  final val SubtypeUnifier: shapeless.ops.hlist.SubtypeUnifier.type = shapeless.ops.hlist.SubtypeUnifier

  @annotation.nowarn("msg=type parameter Lub.*shadows")
  final type ToArray[L <: HList, Lub] = ToTraversable.Aux[L, Array, Lub]
  @annotation.nowarn("msg=type parameter Lub.*shadows")
  final def ToArray[L <: HList, Lub](implicit t: ToArray[L, Lub]): ToArray[L, Lub] = t

  @annotation.nowarn("msg=type parameter Lub.*shadows")
  final type ToList[L <: HList, Lub] = ToTraversable.Aux[L, List, Lub]
  @annotation.nowarn("msg=type parameter Lub.*shadows")
  final def ToList[L <: HList, Lub](implicit t: ToList[L, Lub]): ToList[L, Lub] = t

  final type ToTraversable[L <: HList, M[_]] = shapeless.ops.hlist.ToTraversable[L, M]
  final val ToTraversable: shapeless.ops.hlist.ToTraversable.type = shapeless.ops.hlist.ToTraversable

  final type Transposer[L <: HList] = shapeless.ops.hlist.Transposer[L]
  final val Transposer: shapeless.ops.hlist.Transposer.type = shapeless.ops.hlist.Transposer

  final type Tupler[L <: HList] = shapeless.ops.hlist.Tupler[L]
  final val Tupler: shapeless.ops.hlist.Tupler.type = shapeless.ops.hlist.Tupler

  final type Unifier[T <: HList] = shapeless.ops.hlist.Unifier[T]
  final val Unifier: shapeless.ops.hlist.Unifier.type = shapeless.ops.hlist.Unifier

  final type Union[L <: HList, M <: HList] = shapeless.ops.hlist.Union[L, M]
  final val Union: shapeless.ops.hlist.Union.type = shapeless.ops.hlist.Union

  final type Unzip[L <: HList] = shapeless.ops.hlist.Unzip[L]
  final val Unzip: shapeless.ops.hlist.Unzip.type = shapeless.ops.hlist.Unzip

  final type Zip[L <: HList] = shapeless.ops.hlist.Zip[L]
  final val Zip: shapeless.ops.hlist.Zip.type = shapeless.ops.hlist.Zip

  final type ZipApply[FL <: HList, AL <: HList] = shapeless.ops.hlist.ZipApply[FL, AL]
  final val ZipApply: shapeless.ops.hlist.ZipApply.type = shapeless.ops.hlist.ZipApply

  final type ZipConst[C, T <: HList] = shapeless.ops.hlist.ZipConst[C, T]
  final val ZipConst: shapeless.ops.hlist.ZipConst.type = shapeless.ops.hlist.ZipConst

  final type ZipOne[H <: HList, T <: HList] = shapeless.ops.hlist.ZipOne[H, T]
  final val ZipOne: shapeless.ops.hlist.ZipOne.type = shapeless.ops.hlist.ZipOne

  final type ZipWith[L <: HList, R <: HList, F <: shapeless.Poly2] = shapeless.ops.hlist.ZipWith[L, R, F]
  final val ZipWith: shapeless.ops.hlist.ZipWith.type = shapeless.ops.hlist.ZipWith

  final type ZipWithKeys[K <: HList, V <: HList] = shapeless.ops.hlist.ZipWithKeys[K, V]
  final val ZipWithKeys: shapeless.ops.hlist.ZipWithKeys.type = shapeless.ops.hlist.ZipWithKeys

  final type DepFn0 = shapeless.DepFn0
  final type DepFn1[T] = shapeless.DepFn1[T]
  final type DepFn2[T, U] = shapeless.DepFn2[T, U]

  final type Case[F, L <: HList] = shapeless.poly.Case[F, L]
  final val Case: shapeless.poly.Case.type = shapeless.poly.Case

  final type Case0[F] = shapeless.poly.Case0[F]
  final val Case0: shapeless.poly.Case0.type = shapeless.poly.Case0

  final type Case1[F, A] = shapeless.poly.Case1[F, A]
  final val Case1: shapeless.poly.Case1.type = shapeless.poly.Case1

  final type Case2[F, A, B] = shapeless.poly.Case2[F, A, B]
  final val Case2: shapeless.poly.Case2.type = shapeless.poly.Case2

  final type Poly = shapeless.Poly
  final val Poly: shapeless.Poly.type = shapeless.Poly

  final type Poly0 = shapeless.Poly0

  final type Poly1 = shapeless.Poly1
  final val Poly1: shapeless.Poly1.type = shapeless.Poly1

  final type Poly2 = shapeless.Poly2
  final val Poly2: shapeless.Poly2.type = shapeless.Poly2

  final type ->[T, R] = shapeless.poly.->[T, R]
  final type >->[T, R] = shapeless.poly.>->[T, R]

  final val tupled: shapeless.tupled.type = shapeless.tupled
  final val productElements: shapeless.productElements.type = shapeless.productElements
}
