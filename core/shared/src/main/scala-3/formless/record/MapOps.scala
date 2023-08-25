package formless.record

final class FormlessMapOps[K, V](private val m: Map[K, V]) extends AnyVal {
  final def toRecord[R](using f: FromMap[R]): Option[R] = f(m)
}
