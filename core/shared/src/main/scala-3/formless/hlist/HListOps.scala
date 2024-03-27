package formless.hlist

final class ReinsertAux[L, O](private val l: L) extends AnyVal {
  final def apply[U](u: U)(using r: Remove[O, U], ev: (U, L) =:= r.Out): O = r.reinsert((u, l))
}

final class ReinsertAllAux[L, O](private val l: L) extends AnyVal {
  final def apply[SL](sl: SL)(using r: RemoveAll.Aux[O, SL, (SL, L)]): O = r.reinsert((sl, l))
}

final class ReplaceTypeAux[L, U](private val l: L) extends AnyVal {
  final def apply[V](v: V)(using r: Replacer[L, U, V]): r.Out = r(l, v)
}

final class UpdatedTypeAux[L, U](private val l: L) extends AnyVal {
  final def apply[V, Out <: HList](v: V)(using r: Replacer.Aux[L, U, V, (U, Out)]): Out = r(l, v)._2
}

final class UpdatedAtAux[L, N](private val l: L) extends AnyVal {
  final def apply[U, V, Out <: HList](u: U)(using r: ReplaceAt.Aux[L, N, U, (V, Out)]): Out = r(l, u)._2
}

final class PatchAux[L, N, M](private val l: L) extends AnyVal {
  final def apply[In <: HList](in: In)(using p: Patcher[N, M, L, In]): p.Out = p(l, in)
}

final class HListOps[L <: HList](private val l: L) extends AnyVal {
  /**
   * Prepend the argument element to this `HList`.
   */
  def ::[H](h : H): H :: L = formless.hlist.::(h, l)

  /**
   * Prepend the argument element to this `HList`.
   */
  def +:[H](h : H): H :: L = formless.hlist.::(h, l)

  /**
   * Append the argument element to this `HList`.
   */
  final def :+[T](t: T)(using p: Prepend[L, T :: HNil]): p.Out = p(l, t :: HNil)

  /**
   * Append the argument `HList` to this `HList`.
   */
  final def ++[S <: HList](suffix: S)(using p: Prepend[L, S]): p.Out = p(l, suffix)

  /**
   * Prepend the argument `HList` to this `HList`.
   */
  final def ++:[P <: HList](prefix: P)(using p: Prepend[P, L]): p.Out = p(prefix, l)

  /**
   * Prepend the argument `HList` to this `HList`.
   */
  final def :::[P <: HList](prefix: P)(using p: Prepend[P, L]): p.Out = p(prefix, l)

  /**
   * Prepend the reverse of the argument `HList` to this `HList`.
   */
  final def reverse_:::[P <: HList](prefix: P)(using p: ReversePrepend[P, L]): p.Out = p(prefix, l)

  /**
   * Returns the `N`th element of this `HList`. An explicit type argument must be provided. Available only if there is
   * evidence that this `HList` has at least `N` elements.
   */
  // TODO - this doesn't work b/c `HList#apply` already exists in scala 3
  final def apply[N <: Int](using n: ValueOf[N]): HList.Elem[L, N] = l.unsafeApply(n.value).asInstanceOf[HList.Elem[L, N]]

  /**
   * Returns the `N`th element of this `HList`. Available only if there is evidence that this `HList` has at least `N`
   * elements.
   */
  final def apply(n: Int): HList.Elem[L, n.type] = l.unsafeApply(n).asInstanceOf[HList.Elem[L, n.type]]

  /**
   * Returns the `N`th element of this `HList`. An explicit type argument must be provided. Available only if there is
   * evidence that this `HList` has at least `N` elements.
   */
  final def at[N <: Int](using n: ValueOf[N]): HList.Elem[L, N] = l.unsafeApply(n.value).asInstanceOf[HList.Elem[L, N]]

  /**
   * Returns the `N`th element of this `HList`. Available only if there is evidence that this `HList` has at least `N`
   * elements.
   */
  final def at(n: Int): HList.Elem[L, n.type] = l.unsafeApply(n).asInstanceOf[HList.Elem[L, n.type]]

  /**
   * Returns the last element of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  final def last(using lst: Last[L]): lst.Out = lst(l)

  /**
   * Returns a `HList` consisting of all the elements of this `HList` except the last. Available only if there is
   * evidence that this `HList` is composite.
   */
  final def init(using i: Init[L]): i.Out = i(l)

  /**
   * Returns the first element of type `U` of this `HList`. An explicit type argument must be provided. Available only
   * if there is evidence that this `HList` has an element of type `U`.
   */
  final def select[U](using s: Selector[L, U]): U = s(l)

  final def selectManyType[Ids <: HList](using s: SelectMany[L, Ids]): s.Out = s(l)

  // TODO - this doesn't work well because e.g. `0 :: HNil` is typed as `Int :: HNil`
  // instead of a preserving the singleton type of `0 :: HNil`
  final def selectMany[Ids <: HList](ids: Ids)(using s: SelectMany[L, Ids]): s.Out = s(l)

  /**
   * Returns the elements of this `HList` specified by the range of ids in [A,B[
   * Available only if there is evidence that this `HList` contains all elements in that range
   */
  final def selectRange[A <: Int, B <: Int](using s: SelectRange[L,A,B]): s.Out = s(l)

  final def selectRange(a: Int, b: Int)(using s: SelectRange[L, a.type, b.type]): s.Out = s(l)

  /**
   * Returns all elements of type `U` of this `HList`. An explicit type argument must be provided.
   */
  final def filter[U](using p: Partition[L, U]): p.Prefix  = p.filter(l)

  /**
   * Returns all elements of type different than `U` of this `HList`. An explicit type argument must be provided.
   */
  final def filterNot[U](using p: Partition[L, U]): p.Suffix  = p.filterNot(l)

  final def partition[U](using p: Partition[L, U]): (p.Prefix, p.Suffix) = p(l)

  /**
   * Returns the first element of type `U` of this `HList` plus the remainder of the `HList`. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has an element of type `U`.
   *
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#remove and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  final def removeElem[U](using r: Remove[L, U]): r.Out = r(l)

  /**
   * Returns the first elements of this `HList` that have types in `SL` plus the remainder of the `HList`. An expicit
   * type argument must be provided. Available only if there is evidence that this `HList` contains elements with
   * types in `SL`.
   */
  final def removeAll[SL <: HList](using r: RemoveAll[L, SL]): r.Out = r(l)

  /**
   * Returns the union between this `HList` and another `HList`. In case of duplicate types, this operation is a
   * order-preserving multi-set union. If type `T` appears n times in this `HList` and m > n times in `M`, the
   * resulting `HList` contains the first n elements of type `T` in this `HList`, followed by the last m - n element
   * of type `T` in `M`.
   */
  final def union[M](m: M)(using u: Union[L, M]): u.Out = u(l, m)

  /**
   * Returns the intersection between this `HList` and another `HList`. In case of duplicate types, this operation is a
   * multiset intersection. If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList`
   * contains the first m elements of type `T` in this `HList`.
   * Also available if `M` contains types absent in this `HList`.
   */
  final def intersect[M](using i: Intersection[L, M]): i.Out = i(l)

  /**
   * Returns the difference between this `HList` and another `HList`. In case of duplicate types, this operation is a
   * multiset difference. If type `T` appears n times in this `HList` and m < n times in `M`, the resulting `HList`
   * contains the last n - m elements of type `T` in this `HList`.
   * Also available if `M` contains types absent in this `HList`.
   */
  final def diff[M](using d: Diff[L, M]): d.Out = d(l)

  /**
   * Reinserts an element `U` into this `HList` to return another `HList` `O`.
   */
  final def reinsert[O]: ReinsertAux[L, O] = new ReinsertAux[L, O](l)

  /**
   * Reinserts the elements of `SL` into this `HList` to return another `HList` `O`.
   */
  final def reinsertAll[O]: ReinsertAllAux[L, O] = new ReinsertAllAux[L, O](l)

  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value, also of type `U` returning both
   * the replaced element and the updated `HList`. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  final def replace[U](u: U)(using r: Replacer[L, U, U]): r.Out = r(l, u)

  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value of type `V`, returning both the
   * replaced element and the updated `HList`. An explicit type argument must be provided for `U`. Available only if
   * there is evidence that this `HList` has an element of type `U`.
   */
  final def replaceType[U]: ReplaceTypeAux[L, U] = new ReplaceTypeAux[L, U](l)

  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value, also of type `U`. Available only
   * if there is evidence that this `HList` has an element of type `U`.
   *
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#updated and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  final def updatedElem[U, Out <: HList](u: U)(using r: Replacer.Aux[L, U, U, (U, Out)]): Out = r(l, u)._2

  /**
   * Replaces the first element of type `U` of this `HList` with the result of its transformation to a `V` via the
   * supplied function. Available only if there is evidence that this `HList` has an element of type `U`.
   */
  final def updateTypeWith[U, V, Out <: HList](f: U => V)(using m: Modifier.Aux[L, U, V, (U, Out)]): Out = m(l, f)._2

  /**
   * Replaces the `N`th element of this `HList` with the result of calling the supplied function on it.
   * Available only if there is evidence that this `HList` has `N` elements.
   */
  def updateAtWith[V](n: Int)(f: HList.Elem[L, n.type] => V)(
    using m: ModifierAt[L, n.type, HList.Elem[L, n.type], V],
  ): m.Out = m(l, f)

  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value of type `V`. An explicit type
   * argument must be provided for `U`. Available only if there is evidence that this `HList` has an element of
   * type `U`.
   */
  final def updatedType[U]: UpdatedTypeAux[L, U] = new UpdatedTypeAux[L, U](l)

  /**
   * Replaces the `N`th element of this `HList` with the supplied value of type `U`. An explicit type argument
   * must be provided for `N`. Available only if there is evidence that this `HList` has at least `N` elements.
   */
  final def updatedAt[N]: UpdatedAtAux[L, N] = new UpdatedAtAux[L, N](l)

  /**
   * Replaces the `n`th element of this `HList` with the supplied value of type `U`. Available only if there is
   * evidence that this `HList` has at least `n` elements.
   */
  final def updatedAt[U, V, Out <: HList](n: Int, u: U)(using r: ReplaceAt.Aux[L, n.type, U, (V, Out)]): Out = r(l, u)._2

  /**
   * Returns the first `N` elements of this `HList`. An explicit type argument must be provided. Available only if
   * there is evidence that this `HList` has at least `N` elements.
   */
  final def take[N](using t: Take[L, N]): t.Out = t(l)

  /**
   * Returns the first `n` elements of this `HList`. Available only if there is evidence that this `HList` has at
   * least `n` elements.
   */
  final def take(n: Int)(using t: Take[L, n.type]): t.Out = t(l)

  /**
   * Returns all but the  first `N` elements of this `HList`. An explicit type argument must be provided. Available
   * only if there is evidence that this `HList` has at least `N` elements.
   */
  final def drop[N](using d: Drop[L, N]): d.Out = d(l)

  /**
   * Returns all but the  first `n` elements of this `HList`. Available only if there is evidence that this `HList`
   * has at least `n` elements.
   */
  final def drop(n: Int)(using d: Drop[L, n.type]): d.Out = d(l)

  /**
   * Splits this `HList` at the `N`th element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has at least `N` elements.
   */
  final def split[N](using s: Split[L, N]): s.Out = s(l)

  /**
   * Splits this `HList` at the `n`th element, returning the prefix and suffix as a pair. Available only if there is
   * evidence that this `HList` has at least `n` elements.
   */
  final def split(n: Int)(using s: Split[L, n.type]): s.Out = s(l)

  /**
   * Splits this `HList` at the `N`th element, returning the reverse of the prefix and suffix as a pair. An explicit
   * type argument must be provided. Available only if there is evidence that this `HList` has at least `N` elements.
   */
  final def reverse_split[N](using s: ReverseSplit[L, N]): s.Out = s(l)

  /**
   * Splits this `HList` at the `n`th element, returning the reverse of the prefix and suffix as a pair. Available
   * only if there is evidence that this `HList` has at least `n` elements.
   */
  final def reverse_split(n: Int)(using s: ReverseSplit[L, n.type]): s.Out = s(l)

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  final def splitLeft[U](using s: SplitLeft[L, U]): s.Out = s(l)

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  final def reverse_splitLeft[U](using s: ReverseSplitLeft[L, U]): s.Out = s(l)

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  final def splitRight[U](using s: SplitRight[L, U]): s.Out = s(l)

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  final def reverse_splitRight[U](using s: ReverseSplitRight[L, U]): s.Out = s(l)

  /**
   * Permutes this `HList` into the same order as another `HList`. An explicit type argument must be supplied.
   * Available only if both `HList`s have elements of the same types.
   */
  final def align[M <: HList](using a: Align[L, M]): M = a(l)

  /**
   * Permutes this `HList` into the same order as the supplied `HList` with the same element types. Available only if
   * both `HList`s have elements of the same types.
   */
  final def align[M <: HList](m: M)(using a: Align[L, M]): M = a(l)

  /**
   * Reverses this `HList`.
   */
  final def reverse_(using r: Reverse[L]): r.Out = r(l)

  /**
   * Maps a higher rank function across this `HList`.
   */
  final def map(f: Poly)(using m: Mapper[f.type, L]): m.Out = m(l)

  /**
   * Flatmaps a higher rank function across this `HList`.
   */
  final def flatMap(f: Poly)(using m: FlatMapper[f.type, L]): m.Out = m(l)

  /**
   * Conses an element onto each row of this matrix (`HList` of `HList`s).
   */
  final def mapCons[A](a: A)(using m: MapCons[A, L]): m.Out = m(a, l)

  /**
   * Replaces each element of this `HList` with a constant value.
   */
  final def mapConst[C](c: C)(using m: ConstMapper[C, L]): m.Out = m(c, l)

  /**
   * Collect a higher rank function across this `HList`.
   */
  final def collect(p: Poly)(using c: Collect[L, p.type]): c.Out = c(l)

  /**
   * Maps a higher rank function `f` across this `HList` and folds the result using monomorphic combining operator
   * `op`. Available only if there is evidence that the result type of `f` at each element conforms to the argument
   * type of `op`.
   */
  final def foldMap[R](z: R)(f: Poly)(op: (R, R) => R)(using folder: MapFolder[L, R, f.type]): R = folder(l, z, op)

  /**
   * Computes a left fold over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  final def foldLeft[R](z: R)(op: Poly)(using folder: LeftFolder[L, R, op.type]): folder.Out = folder(l, z)

  /**
   * Computes a right fold over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  final def foldRight[A](a: A)(op: Poly)(using f: RightFolder[L, A, op.type]): f.Out = f(l, a)

  /**
   * Computes a left reduce over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `HList` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  final def reduceLeft(op: Poly)(using r: LeftReducer[L, op.type]): r.Out = r(l)

  /**
   * Computes a right reduce over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `HList` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  final def reduceRight(op: Poly)(using r: RightReducer[L, op.type]): r.Out = r(l)

  /**
    * Repeats this `HList` N times.
    */
  final def repeat[N](using r: Repeat[L, N]): r.Out = r(l)

  /**
   * Zips this `HList` with its argument `HList` returning an `HList` of pairs.
   */
  final def zip[R <: HList](r: R)(using z: Zip[L :: R :: HNil]): z.Out = z(l :: r :: HNil)

  /**
   * Unzips this `HList` of tuples returning a tuple of `HList`s. Available only if there is evidence that this
   * `HList` has tuple elements.
   */
  final def unzip(using u: Unzip[L]): u.Out = u(l)

  /**
   * Zips this `HList` of monomorphic function values with its argument `HList` of correspondingly typed function
   * arguments returning the result of each application as a `HList`. Available only if there is evidence that the
   * corresponding function and argument elements have compatible types.
   */
  final def zipApply[A <: HList](a: A)(using z: ZipApply[L, A]): z.Out = z(l, a)

  /**
   * Zips this `HList` with its argument `HList` of `HList`s, returning a `HList` of `HList`s with each element of
   * this `HList` prepended to the corresponding `HList` element of the argument `HList`.
   */
  final def zipOne[T <: HList](t: T)(using z: ZipOne[L, T]): z.Out = z(l, t)

  /**
   * Zips this `HList` with a constant, resulting in a `HList` of `HList`s of the form
   * ({element from this `HList`}, {supplied constant})
   */
  final def zipConst[C](c: C)(using z: ZipConst[C, L]): z.Out = z(c, l)

  /**
   * Zips this `HList` with its argument `HList` using argument `Poly2`, returning an `HList`.
   * Doesn't require this to be the same length as its `HList` argument, but does require evidence that its
   * `Poly2` argument is defined at their intersection.
   */
  final def zipWith[R <: HList, P <: Poly2](r: R)(p: P)(using z: ZipWith[L, R, P]): z.Out = z(l, r)

  /**
   * Zips this `HList` with its element indices,  resulting in a `HList` of  `HList`s of the form
   * ({element from input `HList`}, {element index})
   */
  final def zipWithIndex(using z: ZipWithIndex[L]): z.Out = z(l)

  /**
   * Transposes this `HList`.
   */
  final def transpose(using t: Transposer[L]): t.Out = t(l)

  /**
   * Returns a `HList` typed as a repetition of the least upper bound of the types of the elements of this `HList`.
   */
  final def unify(using u: Unifier[L]): u.Out = u(l)

  /**
   * Returns a `HList` with all elements that are subtypes of `B` typed as `B`.
   */
  final def unifySubtypes[B](using u: SubtypeUnifier[L, B]): u.Out = u(l)

  /**
   * Converts this `HList` to a correspondingly typed tuple.
   */
  final def tupled(using t: Tupler[L]): t.Out = t(l)

  /**
   * Compute the length of this `HList`.
   */
  final def length(using len: Length[L]): len.Out = len()

  /**
   * Compute the length of this `HList` as a runtime Int value.
   */
  final def runtimeLength: Int = {
    @annotation.tailrec def loop(l: HList, acc: Int): Int = l match {
      case HNil => acc
      case _ :: t => loop(t, acc + 1)
    }

    loop(l, 0)
  }

  /**
   * Convert this `HList` to a `List[Any]`.
   */
  final def runtimeList: List[Any] = {
    val builder = List.newBuilder[Any]

    @annotation.tailrec def loop(l: HList): Unit = l match {
      case HNil => ()
      case h :: t =>
        builder += h
        loop(t)
    }

    loop(l)
    builder.result()
  }

  /**
   * Converts this `HList` to a `M` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  final def to[M[_]](using t: ToTraversable[L, M]): t.Out = t(l)

  /**
   * Converts this `HList` to a `M` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  final def toLub[M[_], Lub](using t: ToTraversable.Aux[L, M, Lub]): t.Out = t(l)

  /**
   * Converts this `HList` to an ordinary `List` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  final def toList[Lub](using tl: ToList[L, Lub]): tl.Out = tl(l)

  /**
   * Converts this `HList` to an `Array` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   *
   * It is advisable to specify the type parameter explicitly, because for many reference types, case classes in
   * particular, the inferred type will be too precise (ie. `Product with Serializable with CC` for a typical case class
   * `CC`) which interacts badly with the invariance of `Array`s.
   */
  final def toArrayLub[Lub](using ta: ToArray[L, Lub]): ta.Out = ta(l)

  /**
   * Displays all elements of this HList in a string using start, end, and separator strings.
    */
  final def mkString(start: String, sep: String, end: String): String = {
    @annotation.tailrec
    def go(acc: String, sub: HList): String = sub match {
      case HNil => ""
      case h :: HNil => acc ++ h.toString
      case h :: t => go(acc ++ h.toString + sep, t)
    }

    go(start, l) + end
  }


  /**
   * Converts this `HList` of values into a record with the provided keys.
   */
  final def zipWithKeys[K <: HList](keys: K)(using z: ZipWithKeys[K, L]): z.Out = z(l)

  /**
   * Converts this `HList` of values into a record with given keys. A type argument must be provided.
   */
  final def zipWithKeys[K <: HList](using z: ZipWithKeys[K, L]): z.Out = z(l)

  /**
   * Returns all permutations of this `HList`
   */
  final def permutations(using p: Permutations[L]): p.Out = p(l)

  /**
   * Rotate this `HList` left by N. An explicit type argument must be provided.
   */
  final def rotateLeft[N](using r: RotateLeft[L, N]): r.Out = r(l)

  /**
   * Rotate this `HList` left by N
   */
  final def rotateLeft(n: Int)(using r: RotateLeft[L, n.type]): r.Out = r(l)

  /**
   * Rotate this `HList` right by N. An explicit type argument must be provided.
   */
  final def rotateRight[N](using r: RotateRight[L, N]): r.Out = r(l)

  /**
   * Rotate this `HList` right by N
   */
  final def rotateRight(n: Int)(using r: RotateRight[L, n.type]): r.Out = r(l)

  /**
   * Computes a left scan over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the results of the appropriate types.
   */
  final def scanLeft[A](z: A)(op: Poly)(using s: LeftScanner[L, A, op.type]): s.Out = s(l, z)

  /**
   * Computes a right scan over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the results of the appropriate types.
   */
  final def scanRight[A](z: A)(op: Poly)(using s: RightScanner[L, A, op.type]): s.Out = s(l, z)

  /**
   *
   * Produces a new `HList` where a slice of this `HList` is replaced by another. Available only if there are at least
   * ``n`` plus ``m`` elements.
   */
  final def patch[In <: HList](n: Int, in: In, m: Int)(using p: Patcher[n.type, m.type, L, In]): p.Out = p(l, in)

  /**
   * Produces a new `HList` where a slice of this `HList` is replaced by another. Two explicit type arguments must be
   * provided. Available only if there are at least `N` plus `M` elements.
   */
  final def patch[N, M]: PatchAux[L, N, M] = new PatchAux[L, N, M](l)

  /**
   * Finds the first element of the HList for which the given Poly is defined, and applies the Poly to it.
   */
  final def collectFirst[P <: Poly](p: P)(using c: CollectFirst[L, p.type]): c.Out = c(l)

  /**
   * Groups the elements of this `HList` into `HList`s of `n` elements, offset by `step`
   */
  final def group(n: Int, step: Int)(using g: Grouper[L, n.type, step.type]): g.Out = g(l)

  /**
   * Appends `elem` until a given length `N` is reached.
   */
  final def padTo[A](n: Int, elem: A)(using p: PadTo[n.type, A, L]): p.Out = p(elem, l)

  /**
   * Slices beginning at index `from` and afterwards, up until index `until`
   */
  final def slice(from: Int, until: Int)(using s: Slice[from.type, until.type, L]): s.Out = s(l)

  /**
   * Returns all combinations of exactly length `N` of elements from this `HList`
   */
  final def combinations(n: Int)(using c: Combinations[n.type, L]): c.Out = c(l)
}
