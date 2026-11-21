package formless

import formless.hlist.*
import formless.record.{->>, FieldOp, LacksKey, Remover, Selector, Updater}
import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

object LookupCrashTests {
  trait Cyclic { type L <: Int :: L }
  trait Growing { type L[A] <: Int :: L[List[A]] }
}

/** Lookups that once crashed the compiler, or never finished, are refused with a compile error instead */
class LookupCrashTests extends FunSuite {
  import LookupCrashTests.*

  inline def refused(inline code: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.nonEmpty, s"expected a compile error: $code")
    assert(!errs.exists(_.contains("Exception occurred while executing macro expansion")), errs.mkString("\n"))
  }

  inline def abortsWith(inline code: String, expected: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.exists(_.contains(expected)), errs.mkString("\n"))
  }

  test("list types whose bounds refer to themselves are refused") {
    refused("""def f(c: Cyclic) = Selector[c.L, "a"]""")
    refused("""def f(c: Cyclic) = LacksKey[c.L, "a"]""")
    refused("""def f(c: Growing) = Updater[c.L[Int], "a" ->> Int]""")
    refused("""def f(c: Growing) = NotContains[c.L[Int], String]""")
    refused("""def f(c: Growing) = ReverseSplitRight[c.L[Int], Int]""")
  }

  test("lookups inside a method without a result type are refused cleanly, for every operation") {
    refused("""def f[H] = Remove[H :: HNil, Int]""")
    refused("""def f[H] = Replacer[H :: HNil, Int, String]""")
    refused("""def f[H] = SplitLeft[H :: HNil, Int]""")
    refused("""def f[H] = SplitRight[H :: Int :: H :: HNil, Int]""")
    refused("""def f[H] = NotContains[H :: HNil, Int]""")
    refused("""def f[H] = Updater[H :: HNil, "a" ->> Int]""")
    refused("""def f[V] = LacksKey[("a" ->> V) :: HNil, "a"]""")
    refused("""def f[H] = Remover[H :: HNil, "a"]""")
  }

  test("a value whose type refers to itself is refused cleanly") {
    refused("""object O { val k = LacksKey[(k.type ->> Int) :: HNil, "b"] }""")
    refused("""object O { val k = Remove[k.type :: HNil, Int] }""")
  }

  test("operations the macros don't know are refused") {
    refused("""ElemOp.elemOpInst[Int :: HNil, Int, String]""")
    refused("""FieldOp.fieldOpInst[("a" ->> Int) :: HNil, "a" ->> Any, String]""")
  }

  test("Remove keeps the arms of a union of lists apart, so reinsert only takes what it can rebuild") {
    type L = (String :: Int :: String :: HNil) | (Boolean :: Int :: Boolean :: HNil)
    val r = Remove[L, Int]
    summon[r.Out =:= (Int, (String :: String :: HNil) | (Boolean :: Boolean :: HNil))]
    assertEquals(r.reinsert((42, "x" :: "y" :: HNil)), "x" :: 42 :: "y" :: HNil)
    refused("""val r = Remove[(String :: Int :: String :: HNil) | (Boolean :: Int :: Boolean :: HNil), Int]; r.reinsert((42, "x" :: true :: HNil))""")
  }

  test("Remove is refused for an element of an abstract list's upper bound, which reinsert couldn't rebuild") {
    refused("""def f[L <: Int :: String :: HNil] = Remove[L, String]""")
    // An element before an abstract tail is the list's own, so it can be put back
    assertEquals(typeCheckErrors("""def f[T <: HList] = Remove[Int :: String :: T, String]""").map(_.message), Nil)
  }
}
