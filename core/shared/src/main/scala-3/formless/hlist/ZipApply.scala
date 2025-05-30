package formless.hlist

/**
 * Type class supporting zipping this `HList` of monomorphic function values with its argument `HList` of
 * correspondingly typed function arguments returning the result of each application as a `HList`. Available only if
 * there is evidence that the corresponding function and argument elements have compatible types.
 */
trait ZipApply[FL, AL] extends DepFn2[FL, AL], Serializable

object ZipApply {
  type Aux[FL, AL, O] = ZipApply[FL, AL] { type Out = O }

  inline def apply[FL, AL](using z: ZipApply[FL, AL]): ZipApply.Aux[FL, AL, z.Out] = z

  given zipApplyHNil: ZipApply.Aux[HNil, HNil, HNil] =
    new ZipApply[HNil, HNil] {
      type Out = HNil
      def apply(fl: HNil, al: HNil): Out = HNil
    }

  given zipApplyHCons[T, R, FLT <: HList, ALT <: HList, ZttOut <: HList](
    using ztt: ZipApply.Aux[FLT, ALT, ZttOut],
  ): ZipApply.Aux[(T => R) :: FLT, T :: ALT, R :: ZttOut] =
    new ZipApply[(T => R) :: FLT, T :: ALT] {
      type Out = R :: ZttOut
      def apply(fl: (T => R) :: FLT, al: T :: ALT): Out = fl.head(al.head) :: ztt(fl.tail, al.tail)
    }
}
