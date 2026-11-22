package formless.record

import formless.hlist.{DepFn2, HList}

/**
 * Type class supporting modification of a record field by given function.
 */
@scala.annotation.implicitNotFound("Cannot modify field ${K} of record ${T} with a function from ${A}: the lookup requires a field of type ${K} ->> ${A}, and refuses to skip a field with the same key and a strictly narrower type. The match could also be in an abstract part of the record or differ between the arms of a union")
trait Modifier[T <: HList, K, A, B] extends DepFn2[T, A => B], Serializable

object Modifier {
  type Aux[T <: HList, K, A, B, O] = Modifier[T, K, A, B] { type Out = O }

  inline def apply[T <: HList, K, A, B](using m: Modifier[T, K, A, B]): Modifier.Aux[T, K, A, B, m.Out] = m

  given modifierAt[T <: HList, K, A, B](using op: ReplaceAt[T, K ->> A, ReplaceAt.Field]): Modifier.Aux[T, K, A, B, op.Replaced[K ->> B]] =
    new Modifier[T, K, A, B] {
      type Out = op.Replaced[K ->> B]
      def apply(t: T, f: A => B): Out = HList.updatedAt(t, op.index, f).asInstanceOf[Out]
    }
}
