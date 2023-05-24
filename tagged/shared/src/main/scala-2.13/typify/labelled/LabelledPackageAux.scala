package typify
package labelled

import scala.language.experimental.macros
import scala.language.implicitConversions
import typify.tuple.Tuple

final class LabelledOps[K, V](private val labelled: K ->> V) extends AnyVal {
  @inline final def label(implicit k: ValueOf[K]): K = k.value
}

private[typify] trait LabelledPackageAux {
  @inline final implicit def singletonToSingletonOps(t: Any): shapeless.syntax.SingletonOps =
    macro shapeless.SingletonTypeMacros.mkSingletonOps

  @inline final implicit def labelledToLabelledOps[K, V](l: K ->> V): LabelledOps[K, V] =
    new LabelledOps[K, V](l)

  @inline final def tupleToRecordOps[T <: Tuple](t: T): shapeless.syntax.RecordOps[T] =
    new shapeless.syntax.RecordOps[T](t)

  type Selector[T <: Tuple, K] = shapeless.ops.record.Selector[T, K]
  val Selector: shapeless.ops.record.Selector.type = shapeless.ops.record.Selector

  type Updater[L <: Tuple, F] = shapeless.ops.record.Updater[L, F]
  val Updater: shapeless.ops.record.Updater.type = shapeless.ops.record.Updater

  type Remover[T <: Tuple, K] = shapeless.ops.record.Remover[T, K]
  val Remover: shapeless.ops.record.Remover.type = shapeless.ops.record.Remover

  type Merger[L <: Tuple, M <: Tuple] = shapeless.ops.record.Merger[L, M]
  val Merger: shapeless.ops.record.Merger.type = shapeless.ops.record.Merger

  type Keys[T <: Tuple] = shapeless.ops.record.Keys[T]
  val Keys: shapeless.ops.record.Keys.type = shapeless.ops.record.Keys

  type Renamer[T <: Tuple, K1, K2] = shapeless.ops.record.Renamer[T, K1, K2]
  val Renamer: shapeless.ops.record.Renamer.type = shapeless.ops.record.Renamer
}
