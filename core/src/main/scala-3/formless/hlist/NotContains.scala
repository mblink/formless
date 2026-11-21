package formless.hlist

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
@scala.annotation.implicitNotFound("Cannot prove that ${L} has no element of type ${U}: it has one, or one could be in an abstract part of the type (a type parameter or abstract type member)")
trait NotContains[L, U] extends Serializable

object NotContains {
  inline def apply[L, U](using n: NotContains[L, U]): NotContains[L, U] = n

  private val singleton = new NotContains[Any, Any] {}

  given notContainsAbsent[L <: HList, U](using a: formless.record.FieldAbsent[L, U, =:=]): NotContains[L, U] =
    singleton.asInstanceOf[NotContains[L, U]]
}
