package formless.record

import scala.language.dynamics
import formless.hlist.{HList, Poly}

final class FormlessRecordOps[T <: HList](private val t: T) extends AnyVal {
  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   */
  final def get[K <: Singleton](@annotation.nowarn("msg=unused") k: K)(using s: Selector[T, K]): s.Out = s(t)

  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   */
  final def apply[K <: Singleton](@annotation.nowarn("msg=unused") k: K)(using MaybeRecord[T])(using s: Selector[T, K]): s.Out = s(t)

  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   */
  final def fieldAt[K <: Singleton](@annotation.nowarn("msg=unused") k: K)(using s: Selector[T, K]): K ->> s.Out = label[K](s(t))

  /**
   * Replaces a field of the same key and value type, or appends the field if the key is absent.
   */
  final def updated[K <: Singleton, V](@annotation.nowarn("msg=unused") k: K, v: V)(using u: Updater[T, K ->> V]): u.Out = u(t, label[K](v))

  /**
   * Replaces the value of field k with a value of the same type. Only available if this record has a field with
   * keyType equal to the singleton type K and field type equal to K ->> V.
   */
  final def replace[K <: Singleton, V](@annotation.nowarn("msg=unused") k: K, v: V)(using MaybeRecord[T])(
    using s: Selector[T, K] { type Out = V },
    u: Updater[T, K ->> V],
  ): u.Out = u(t, label[K](v))

  /**
   * Not record syntax: `replace` on a record takes a key and a value. It's here so that `replace(value)` on a plain
   * `HList`, the `HList` method this syntax hides, explains why instead of reporting a missing argument.
   */
  final def replace[V](@annotation.nowarn("msg=unused") v: V)(using MaybeRecord[T])(using ev: ReplaceTakesAKey[T]): ev.Out = ev.out

  /**
   * Updates a field with a function from its selected value type. The field's type must match that key and value exactly.
   */
  final def updateWith[K, B](k: K)(using MaybeRecord[T])(using s: Selector[T, k.type])(f: s.Out => B)(using m: Modifier[T, k.type, s.Out, B]): m.Out = m(t, f)

  /**
   * Remove the field associated with the singleton typed key k, returning both the corresponding value and the updated
   * record. Only available if this record has a field with keyType equal to the singleton type K.
   */
  final def remove[K <: Singleton](@annotation.nowarn("msg=unused") k: K)(using r: Remover[T, K]): r.Out = r(t)

  /**
   * Replaces a field of the same key and type, or appends the field if the key is absent.
   */
  final def +[F](f: F)(using u: Updater[T, F]): u.Out = u(t, f)

  /**
   * Remove the field associated with the singleton typed key k, returning the updated record. Only available if this
   * record has a field with keyType equal to the singleton type K.
   */
  final def -[K <: Singleton, R, V, O](@annotation.nowarn("msg=unused") k: K)(using r: Remover[T, K] { type Out = R }, ev: R <:< (V, O)): O = r(t)._2

  /**
   * Merges records whose shared keys have the same value types, taking their values from the right record.
   */
  final def merge[M](m: M)(using mg: Merger[T, M]): mg.Out = mg(t, m)

  /**
    * Extracts super-record from sub-record according to depth subtype relation
    */
  final def extract[E](using e: Extractor[T, E]): E = e(t)

  /**
    * Returns the union of this record and another record using the provided `f` to combine the values of fields which are present in both.
    *
    * The duplicated fields will be merged with `f`.
    */
  final def mergeWith[M](m: M)(f: Poly)(using mw: MergeWith[T, M, f.type]): mw.Out = mw(t, m)

  /**
   * Rename the field associated with the singleton typed key oldKey. Only available if this
   * record has a field with keyType equal to the singleton type K1.
   */
  final def renameField[K1 <: Singleton, K2 <: Singleton](
    @annotation.nowarn("msg=unused") oldKey: K1,
    @annotation.nowarn("msg=unused") newKey: K2,
  )(using r: Renamer[T, K1, K2]): r.Out = r(t)

  /**
   * Returns the keys of this record as a `HList` of singleton typed values.
   */
  final def keys(using k: Keys[T]): k.Out = k()

  /**
   * Returns a `HList` of the values of this record.
   */
  final def values(using v: Values[T]): v.Out = v(t)

  /**
   * Returns a `HList` made of the key-value pairs of this record.
   */
  final def fields(using f: Fields[T]): f.Out = f(t)

  /**
   * Returns a `Map` whose keys and values are typed as the Lub of the keys
   * and values of this record.
   */
  final def toMap[K, V](using tm: ToMap[T] { type Key = K; type Value = V }): Map[K, V] = tm(t)

  /**
   * Maps a higher rank function across the values of this record.
   */
  final def mapValues(f: Poly)(using m: MapValues[f.type, T]): m.Out = m(t)

  /**
    * Align the keys on the order of HList of keys K
    */
  final def alignByKeys[K <: HList](using a: AlignByKeys[T, K]): a.Out = a(t)

  /**
   * Returns a wrapped version of this record that provides `selectDynamic` access to fields.
   */
  final def record: FormlessDynamicRecordOps[T] = new FormlessDynamicRecordOps[T](t)
}

/**
 * Record wrapper providing `selectDynamic` access to fields.
 */
final class FormlessDynamicRecordOps[T <: HList](t: T) extends Dynamic {
  /**
   * Allows dynamic-style access to fields of the record whose keys are `Strings`.
   */
  def selectDynamic(k: String)(using s: Selector[T, k.type]): s.Out = s(t)
}
