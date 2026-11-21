package formless
package record

/**
 * Building fields (`"key" ->> value`, `field`, `label`) and the record syntax on `HList`s, without the record type
 * classes, whose names (`Selector`, `Modifier`, `Remove`, `RemoveAll`, `SelectAll`) are also `HList` type classes. Import
 * `formless.record.syntax.*` instead of `formless.record.*` alongside `formless.hlist.*`.
 */
object syntax {
  export formless.record.{->>, field, label, toFormlessRecordOps, toFormlessMapOps}
}
