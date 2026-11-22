package formless.hlist

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
@scala.annotation.implicitNotFound("Cannot prove that ${L} has no element of type ${U}: it has one, or one could be in an abstract part of the type (a type parameter or abstract type member)")
trait NotContains[L, U] extends Serializable

object NotContains {
  // A noncapturing SAM lets the JVM share a proof without changing the variance of the public type class.
  private trait Proof[L, U] extends NotContains[L, U] with Runnable

  inline def apply[L, U](using n: NotContains[L, U]): NotContains[L, U] = n

  given notContainsAbsent[L <: HList, U](using a: formless.record.FieldAbsent[L, U, =:=]): NotContains[L, U] = {
    val proof: Proof[L, U] = () => ()
    proof
  }
}
