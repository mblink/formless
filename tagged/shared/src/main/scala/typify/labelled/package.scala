package typify

package object labelled extends LabelledPackageAux {
  type ->>[K, +V] = tagged.TranslucentTagged[V, K]

  @inline def label[T]: tagged.TranslucentTagged.Of[T] = tagged.translucentTag[T]
}
