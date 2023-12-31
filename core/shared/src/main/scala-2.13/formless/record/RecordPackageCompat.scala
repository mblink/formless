package formless
package record

import scala.language.implicitConversions
import formless.tuple.Tuple

final class FormlessSingletonOps[K <: Singleton](private val k: K) extends AnyVal {
  @inline final def ->>[V](v: V): K ->> V = label[K](v)
}

final class FormlessLabelledOps[K, V](private val kv: K ->> V) extends AnyVal {
  @inline final def label(implicit k: ValueOf[K]): K = k.value
}

private[formless] trait RecordPackageCompat {
  final type ->>[K, +V] = tagged.TranslucentTagged[V, K]

  final class LabelPartialAp[K] {
    @inline final def apply[V](v: V): K ->> V = shapeless.labelled.field[K].apply[V](v)
  }

  @inline final def field[K]: LabelPartialAp[K] = new LabelPartialAp[K]
  @inline final def label[K]: LabelPartialAp[K] = new LabelPartialAp[K]

  @inline final implicit def toFormlessSingletonOps[K <: Singleton](k: K): FormlessSingletonOps[K] = new FormlessSingletonOps[K](k)
  @inline final implicit def toFormlessLabelledOps[K, V](kv: K ->> V): FormlessLabelledOps[K, V] = new FormlessLabelledOps[K, V](kv)

  @inline final implicit def tupleToRecordOps[T <: Tuple](t: T): shapeless.syntax.RecordOps[T] =
    new shapeless.syntax.RecordOps[T](t)

  final type AlignByKeys[T <: Tuple, K <: Tuple] = shapeless.ops.record.AlignByKeys[T, K]
  final val AlignByKeys: shapeless.ops.record.AlignByKeys.type = shapeless.ops.record.AlignByKeys

  final type Extractor[L <: Tuple, E <: Tuple] = shapeless.ops.record.Extractor[L, E]
  final val Extractor: shapeless.ops.record.Extractor.type = shapeless.ops.record.Extractor

  final type Fields[L <: Tuple] = shapeless.ops.record.Fields[L]
  final val Fields: shapeless.ops.record.Fields.type = shapeless.ops.record.Fields

  final type LacksKey[T <: Tuple, K] = shapeless.ops.record.LacksKey[T, K]
  final val LacksKey: shapeless.ops.record.LacksKey.type = shapeless.ops.record.LacksKey

  final type Keys[T <: Tuple] = shapeless.ops.record.Keys[T]
  final val Keys: shapeless.ops.record.Keys.type = shapeless.ops.record.Keys

  final type MapValues[F, L <: Tuple] = shapeless.ops.record.MapValues[F, L]
  final val MapValues: shapeless.ops.record.MapValues.type = shapeless.ops.record.MapValues

  final type Merger[L <: Tuple, M <: Tuple] = shapeless.ops.record.Merger[L, M]
  final val Merger: shapeless.ops.record.Merger.type = shapeless.ops.record.Merger

  final type MergeWith[L <: Tuple, M <: Tuple, F] = shapeless.ops.record.MergeWith[L, M, F]
  final val MergeWith: shapeless.ops.record.MergeWith.type = shapeless.ops.record.MergeWith

  final type Modifier[T <: Tuple, K, A, B] = shapeless.ops.record.Modifier[T, K, A, B]
  final val Modifier: shapeless.ops.record.Modifier.type = shapeless.ops.record.Modifier

  final type Remove[L <: Tuple, U] = shapeless.ops.record.Remove[L, U]
  final val Remove: shapeless.ops.record.Remove.type = shapeless.ops.record.Remove

  final type RemoveAll[L <: Tuple, A <: Tuple] = shapeless.ops.record.RemoveAll[L, A]
  final val RemoveAll: shapeless.ops.record.RemoveAll.type = shapeless.ops.record.RemoveAll

  final type Remover[T <: Tuple, K] = shapeless.ops.record.Remover[T, K]
  final val Remover: shapeless.ops.record.Remover.type = shapeless.ops.record.Remover

  final type Renamer[T <: Tuple, K1, K2] = shapeless.ops.record.Renamer[T, K1, K2]
  final val Renamer: shapeless.ops.record.Renamer.type = shapeless.ops.record.Renamer

  final type Selector[T <: Tuple, K] = shapeless.ops.record.Selector[T, K]
  final val Selector: shapeless.ops.record.Selector.type = shapeless.ops.record.Selector

  final type SelectAll[L <: Tuple, K <: Tuple] = shapeless.ops.record.SelectAll[L, K]
  final val SelectAll: shapeless.ops.record.SelectAll.type = shapeless.ops.record.SelectAll

  final type SwapRecord[L <: Tuple] = shapeless.ops.record.SwapRecord[L]
  final val SwapRecord: shapeless.ops.record.SwapRecord.type = shapeless.ops.record.SwapRecord

  final type ToMap[L <: Tuple] = shapeless.ops.record.ToMap[L]
  final val ToMap: shapeless.ops.record.ToMap.type = shapeless.ops.record.ToMap

  final type UnzipFields[L <: Tuple] = shapeless.ops.record.UnzipFields[L]
  final val UnzipFields: shapeless.ops.record.UnzipFields.type = shapeless.ops.record.UnzipFields

  final type Updater[T <: Tuple, F] = shapeless.ops.record.Updater[T, F]
  final val Updater: shapeless.ops.record.Updater.type = shapeless.ops.record.Updater

  final type Values[L <: Tuple] = shapeless.ops.record.Values[L]
  final val Values: shapeless.ops.record.Values.type = shapeless.ops.record.Values
}
