package formless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Type class supporting computing the singleton `Int` type of a `shapeless.Nat`.
 */
trait NatToInt[N <: shapeless.Nat] extends hlist.DepFn0 with Serializable { type Out <: Int }

object NatToInt {
  type Aux[N <: shapeless.Nat, O <: Int] = NatToInt[N] { type Out = O }

  def apply[N <: shapeless.Nat](implicit n: NatToInt[N]): NatToInt.Aux[N, n.Out] = n

  implicit def natToIntInst[N <: shapeless.Nat]: NatToInt[N] = macro NatToIntMacros.instImpl[N]
}

private class NatToIntMacros(override val c: Context) extends shapeless.ops.nat.ToIntMacros(c) {
  import c.universe._

  final def instImpl[N <: shapeless.Nat](implicit nTag: WeakTypeTag[N]): Tree = {
    val natType = nTag.tpe.dealias

    @annotation.tailrec
    def go(remType: Type, acc: Int): Int =
      if (remType <:< _0Tpe) acc
      else remType.baseType(succSym) match {
        case TypeRef(pre, _, List(n)) if pre =:= succPre => go(n, acc + 1)
        case _ => abort(s"$natType is not a Nat type")
      }

    val int = Constant(go(natType, 0))
    val intType = internal.constantType(int)

    q"""
      (new _root_.formless.NatToInt[$natType] {
        type Out = $intType
        def apply(): $intType = ${Literal(int)}
      }).asInstanceOf[_root_.formless.NatToInt.Aux[$natType, $intType]]
    """
  }
}
