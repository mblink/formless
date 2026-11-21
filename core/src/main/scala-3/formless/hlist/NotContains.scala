package formless.hlist

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
trait NotContains[L, U] extends Serializable

object NotContains {
  inline def apply[L, U](using n: NotContains[L, U]): NotContains[L, U] = n

  private val singleton = new NotContains[Any, Any] {}

  given notContainsAbsent[L <: HList, U](using a: formless.record.FieldAbsent[L, U, =:=]): NotContains[L, U] =
    singleton.asInstanceOf[NotContains[L, U]]
}
