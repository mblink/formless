package formless
package record

import scala.language.{dynamics, implicitConversions}
import formless.hlist.HList

final class FormlessSingletonOps[K <: Singleton](private val k: K) extends AnyVal {
  @inline final def ->>[V](v: V): K ->> V = label[K](v)
}

final class FormlessLabelledOps[K, V](private val kv: K ->> V) extends AnyVal {
  @inline final def label(implicit k: ValueOf[K]): K = k.value
}

final class FormlessLabelPartialAp[K](private val dummy: Boolean = false) extends AnyVal {
  @inline final def apply[V](v: V): K ->> V = shapeless.labelled.field[K].apply[V](v)
}

final class FormlessDynamicRecordOps[T <: HList](t: T) extends Dynamic {
  def selectDynamic(k: String)(implicit s: Selector[T, k.type]): s.Out = s(t)
}

final class FormlessRecordOps[T <: HList](private val t: T) extends AnyVal {
  final def record: FormlessDynamicRecordOps[T] = new FormlessDynamicRecordOps[T](t)
}

private[formless] sealed trait RecordPackageCompatLP {
  @inline final implicit def hlistToRecordOps[T <: HList](t: T): shapeless.syntax.RecordOps[T] =
    new shapeless.syntax.RecordOps[T](t)
}

private[formless] trait RecordPackageCompat extends RecordPackageCompatLP {
  final type ->>[K, +V] = tagged.TranslucentTagged[V, K]

  final def field[K]: FormlessLabelPartialAp[K] = new FormlessLabelPartialAp[K]
  final def label[K]: FormlessLabelPartialAp[K] = new FormlessLabelPartialAp[K]

  final type FieldPoly = shapeless.FieldPoly
  final type FieldOf[V] = shapeless.FieldOf[V]

  @inline final implicit def toFormlessSingletonOps[K <: Singleton](k: K): FormlessSingletonOps[K] = new FormlessSingletonOps[K](k)
  @inline final implicit def toFormlessLabelledOps[K, V](kv: K ->> V): FormlessLabelledOps[K, V] = new FormlessLabelledOps[K, V](kv)

  @inline final implicit def hlistToFormlessRecordOps[T <: HList](t: T): FormlessRecordOps[T] =
    new FormlessRecordOps[T](t)

  @inline final implicit def toFormlessMapOps[K, V](m: Map[K, V]): shapeless.syntax.std.MapOps[K, V] =
    new shapeless.syntax.std.MapOps(m)

  final type AlignByKeys[T <: HList, K <: HList] = shapeless.ops.record.AlignByKeys[T, K]
  final val AlignByKeys: shapeless.ops.record.AlignByKeys.type = shapeless.ops.record.AlignByKeys

  final type Extractor[L <: HList, E <: HList] = shapeless.ops.record.Extractor[L, E]
  final val Extractor: shapeless.ops.record.Extractor.type = shapeless.ops.record.Extractor

  final type Fields[L <: HList] = shapeless.ops.record.Fields[L]
  final val Fields: shapeless.ops.record.Fields.type = shapeless.ops.record.Fields

  final type LacksKey[T <: HList, K] = shapeless.ops.record.LacksKey[T, K]
  final val LacksKey: shapeless.ops.record.LacksKey.type = shapeless.ops.record.LacksKey

  final type Keys[T <: HList] = shapeless.ops.record.Keys[T]
  final val Keys: shapeless.ops.record.Keys.type = shapeless.ops.record.Keys

  final type MapValues[F, L <: HList] = shapeless.ops.record.MapValues[F, L]
  final val MapValues: shapeless.ops.record.MapValues.type = shapeless.ops.record.MapValues

  final type Merger[L <: HList, M <: HList] = shapeless.ops.record.Merger[L, M]
  final val Merger: shapeless.ops.record.Merger.type = shapeless.ops.record.Merger

  final type MergeWith[L <: HList, M <: HList, F] = shapeless.ops.record.MergeWith[L, M, F]
  final val MergeWith: shapeless.ops.record.MergeWith.type = shapeless.ops.record.MergeWith

  final type Modifier[T <: HList, K, A, B] = shapeless.ops.record.Modifier[T, K, A, B]
  final val Modifier: shapeless.ops.record.Modifier.type = shapeless.ops.record.Modifier

  final type Remove[L <: HList, U] = shapeless.ops.record.Remove[L, U]
  final val Remove: shapeless.ops.record.Remove.type = shapeless.ops.record.Remove

  final type RemoveAll[L <: HList, A <: HList] = shapeless.ops.record.RemoveAll[L, A]
  final val RemoveAll: shapeless.ops.record.RemoveAll.type = shapeless.ops.record.RemoveAll

  final type Remover[T <: HList, K] = shapeless.ops.record.Remover[T, K]
  final val Remover: shapeless.ops.record.Remover.type = shapeless.ops.record.Remover

  final type Renamer[T <: HList, K1, K2] = shapeless.ops.record.Renamer[T, K1, K2]
  final val Renamer: shapeless.ops.record.Renamer.type = shapeless.ops.record.Renamer

  final type Selector[T <: HList, K] = shapeless.ops.record.Selector[T, K]
  final val Selector: shapeless.ops.record.Selector.type = shapeless.ops.record.Selector

  final type SelectAll[L <: HList, K <: HList] = shapeless.ops.record.SelectAll[L, K]
  final val SelectAll: shapeless.ops.record.SelectAll.type = shapeless.ops.record.SelectAll

  final type SwapRecord[L <: HList] = shapeless.ops.record.SwapRecord[L]
  final val SwapRecord: shapeless.ops.record.SwapRecord.type = shapeless.ops.record.SwapRecord

  final type ToMap[L <: HList] = shapeless.ops.record.ToMap[L]
  final val ToMap: shapeless.ops.record.ToMap.type = shapeless.ops.record.ToMap

  final type UnzipFields[L <: HList] = shapeless.ops.record.UnzipFields[L]
  final val UnzipFields: shapeless.ops.record.UnzipFields.type = shapeless.ops.record.UnzipFields

  final type Updater[T <: HList, F] = shapeless.ops.record.Updater[T, F]
  final val Updater: shapeless.ops.record.Updater.type = shapeless.ops.record.Updater

  final type Values[L <: HList] = shapeless.ops.record.Values[L]
  final val Values: shapeless.ops.record.Values.type = shapeless.ops.record.Values
}
