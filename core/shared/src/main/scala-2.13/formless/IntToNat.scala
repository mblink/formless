package formless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Type class supporting computing the `shapeless.Nat` type of a singleton `Int`.
 */
trait IntToNat[I <: Int] extends hlist.DepFn0 with Serializable { type Out <: shapeless.Nat }

object IntToNat {
  type Aux[I <: Int, O <: shapeless.Nat] = IntToNat[I] { type Out = O }

  def apply[I <: Int](implicit i: IntToNat[I]): IntToNat.Aux[I, i.Out] = i

  implicit def intToNatInst[I <: Int]: IntToNat[I] = macro IntToNatMacros.instImpl[I]
}

private class IntToNatMacros(val c: Context) {
  import c.universe._

  final def instImpl[I <: Int](implicit iTag: WeakTypeTag[I]): Tree = {
    val intType = iTag.tpe.dealias

    @annotation.tailrec
    def go(remInt: Int, accType: Tree): (Tree, Tree) =
      remInt match {
        case 0 => (accType, q"new $accType")
        case i => go(i - 1, tq"_root_.shapeless.Succ[$accType]")
      }

    val (natType, natTree) = intType match {
      case ConstantType(Constant(i: Int)) => go(i, tq"_root_.shapeless._0")
      case _ => c.abort(c.enclosingPosition, s"$intType is not a literal Int type")
    }

    q"""
      (new _root_.formless.IntToNat[$intType] {
        type Out = $natType
        def apply(): $natType = $natTree
      }).asInstanceOf[_root_.formless.IntToNat.Aux[$intType, $natType]]
    """
  }
}
