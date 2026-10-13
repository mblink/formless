package formless
package record

import formless.hlist.{::, HList, HNil}
import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

class FindFieldTests extends FunSuite {
  object intKey extends FieldOf[Int]

  type ABCD = ("a" ->> Int) :: ("b" ->> String) :: ("c" ->> Int) :: ("d" ->> Boolean) :: HNil
  val abcd: ABCD = ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: HNil

  // A crash during macro expansion also fails to compile, so only a clean refusal passes
  inline def refused(inline code: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.nonEmpty, s"expected a compile error: $code")
    assert(!errs.exists(_.contains("Exception occurred while executing macro expansion")), errs.mkString("\n"))
  }

  inline def abortsWith(inline code: String, expected: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.exists(_.contains(expected)), errs.mkString("\n"))
  }

  test("select a field whose type is an intersection around `K ->> V`") {
    type R = (("a" ->> Int) & Serializable) :: ("b" ->> String) :: HNil
    val r: R = ("a" ->> 1).asInstanceOf[("a" ->> Int) & Serializable] :: ("b" ->> "x") :: HNil
    val s = Selector[R, "a"]
    summon[s.Out =:= Int]
    assertEquals(s(r), 1)
    assertEquals(Selector[R, "b"].apply(r), "x")
  }

  test("keys of different kinds that look alike are told apart") {
    type R = (1 ->> String) :: ("1" ->> Int) :: intKey.F :: ("k" ->> Double) :: HNil
    val r: R = (1 ->> "int key") :: ("1" ->> 2) :: (intKey ->> 3) :: ("k" ->> 4.0) :: HNil
    assertEquals(Selector[R, 1].apply(r), "int key")
    assertEquals(Selector[R, "1"].apply(r), 2)
    assertEquals(Selector[R, intKey.type].apply(r), 3)
    assertEquals(Selector[R, "k"].apply(r), 4.0)
  }

  test("updating a field in the middle keeps the order and types of the others") {
    val u = Updater[ABCD, "c" ->> Int]
    summon[u.Out =:= ABCD]
    assertEquals(u(abcd, "c" ->> 30), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 30) :: ("d" ->> true) :: HNil)
  }

  test("modifying a field in the middle replaces only its type") {
    val m = Modifier[ABCD, "c", Int, String]
    summon[m.Out =:= (("a" ->> Int) :: ("b" ->> String) :: ("c" ->> String) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(m(abcd, _.toString), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> "3") :: ("d" ->> true) :: HNil)
  }

  test("removing a field in the middle keeps the others in order") {
    val rm = Remover[ABCD, "b"]
    summon[rm.Out =:= (String, ("a" ->> Int) :: ("c" ->> Int) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(rm(abcd), ("two", ("a" ->> 1) :: ("c" ->> 3) :: ("d" ->> true) :: HNil))
  }

  test("a field found before an abstract tail resolves") {
    def b[L <: HList](r: ("b" ->> Int) :: L): Int = Selector[("b" ->> Int) :: L, "b"].apply(r)
    assertEquals(b(("b" ->> 5) :: ("a" ->> 6) :: HNil), 5)
  }

  test("a field that could only be in an abstract tail does not resolve") {
    refused("""def a[L <: HList] = Selector[("b" ->> Int) :: L, "a"]""")
    refused("""def a[L <: HList] = Updater[("b" ->> Int) :: L, "a" ->> Int]""")
    refused("""def a[L <: HList](r: ("b" ->> Int) :: L) = Selector[("b" ->> Int) :: L, "a"].apply(r)""")
    refused("""def a[L <: HList] = Modifier[("b" ->> Int) :: L, "a", Int, Int]""")
    refused("""def a[L <: HList] = Remover[("b" ->> Int) :: L, "a"]""")
  }

  test("absence cannot be proven for a record with an abstract tail") {
    refused("""def a[L <: HList] = LacksKey[("b" ->> Int) :: L, "a"]""")
    refused("""def a[L <: HList] = Updater[("b" ->> Int) :: L, "a" ->> Int]""")
  }

  test("absence is proven for a concrete record lacking the key, and refused when it has it") {
    summon[LacksKey[ABCD, "e"]]
    refused("""LacksKey[ABCD, "c"]""")
    val u = Updater[ABCD, "e" ->> Long]
    assertEquals(
      u(abcd, "e" ->> 5L),
      ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: ("e" ->> 5L) :: HNil,
    )
  }

  def selectA[H <: ("a" ->> Int)](r: H :: HNil) = Selector[H :: HNil, "a"].apply(r)

  test("a field typed as a type parameter takes its key and value from the parameter's bound") {
    assertEquals(typeCheckErrors("""def f[H <: ("a" ->> Int)] = { val s = Selector[H :: HNil, "a"]; summon[s.Out =:= Int] }""").map(_.message), Nil)
    assertEquals(selectA(("a" ->> 1) :: HNil), 1)
  }

  test("a tail that is a subtype of HNil ends the record") {
    summon[LacksKey[("a" ->> Int) :: Nothing, "b"]]
  }

  test("abort messages name the field and the record, even inside a method without a result type") {
    abortsWith("""FindField.findFieldInst[ABCD, "e" ->> Any, <:<]""", "Failed to find field")
    abortsWith("""def f[L <: HList] = FindField.findFieldInst[("b" ->> Int) :: L, "a" ->> Any, <:<]""", "is not statically known")
    abortsWith("""FieldAbsent.fieldAbsentInst[ABCD, "c" ->> Any, <:<]""", "has field")
    abortsWith("""def f[L <: HList] = FieldAbsent.fieldAbsentInst[("b" ->> Int) :: L, "a" ->> Any, <:<]""", "is not statically known")
  }

  test("a plain HList is extended with an element type it lacks, and not with one it has") {
    val u = Updater[Int :: String :: HNil, Boolean]
    summon[u.Out =:= (Int :: String :: Boolean :: HNil)]
    assertEquals(u(1 :: "x" :: HNil, true), 1 :: "x" :: true :: HNil)
    refused("""Updater[Int :: String :: HNil, Int]""")
  }
}
