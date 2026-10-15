package formless
package record

import formless.hlist.{::, HList, HNil}
import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

class FindFieldTests extends FunSuite {
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

  test("updating a field keeps the order and types of the others") {
    val first = Updater[ABCD, "a" ->> Int]
    summon[first.Out =:= ABCD]
    assertEquals(first(abcd, "a" ->> 10), ("a" ->> 10) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: HNil)

    val middle = Updater[ABCD, "c" ->> Int]
    summon[middle.Out =:= ABCD]
    assertEquals(middle(abcd, "c" ->> 30), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 30) :: ("d" ->> true) :: HNil)

    val last = Updater[ABCD, "d" ->> Boolean]
    summon[last.Out =:= ABCD]
    assertEquals(last(abcd, "d" ->> false), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> false) :: HNil)
  }

  test("modifying a field replaces only its type") {
    val first = Modifier[ABCD, "a", Int, Long]
    summon[first.Out =:= (("a" ->> Long) :: ("b" ->> String) :: ("c" ->> Int) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(first(abcd, _.toLong), ("a" ->> 1L) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: HNil)

    val middle = Modifier[ABCD, "c", Int, String]
    summon[middle.Out =:= (("a" ->> Int) :: ("b" ->> String) :: ("c" ->> String) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(middle(abcd, _.toString), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> "3") :: ("d" ->> true) :: HNil)

    val last = Modifier[ABCD, "d", Boolean, Int]
    summon[last.Out =:= (("a" ->> Int) :: ("b" ->> String) :: ("c" ->> Int) :: ("d" ->> Int) :: HNil)]
    assertEquals(last(abcd, b => if (b) 1 else 0), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> 1) :: HNil)
  }

  test("removing a field keeps the others in order") {
    val first = Remover[ABCD, "a"]
    summon[first.Out =:= (Int, ("b" ->> String) :: ("c" ->> Int) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(first(abcd), (1, ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: HNil))

    val middle = Remover[ABCD, "b"]
    summon[middle.Out =:= (String, ("a" ->> Int) :: ("c" ->> Int) :: ("d" ->> Boolean) :: HNil)]
    assertEquals(middle(abcd), ("two", ("a" ->> 1) :: ("c" ->> 3) :: ("d" ->> true) :: HNil))

    val last = Remover[ABCD, "d"]
    summon[last.Out =:= (Boolean, ("a" ->> Int) :: ("b" ->> String) :: ("c" ->> Int) :: HNil)]
    assertEquals(last(abcd), (true, ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: HNil))

    val only = Remover[("a" ->> Int) :: HNil, "a"]
    summon[only.Out =:= (Int, HNil)]
    assertEquals(only(("a" ->> 1) :: HNil), (1, HNil))
  }

  test("the first field with a matching key is the one selected or removed") {
    type R = ("a" ->> Int) :: ("b" ->> Boolean) :: ("a" ->> String) :: HNil
    val r: R = ("a" ->> 1) :: ("b" ->> true) :: ("a" ->> "x") :: HNil

    val s = Selector[R, "a"]
    summon[s.Out =:= Int]
    assertEquals(s(r), 1)

    val rm = Remover[R, "a"]
    summon[rm.Out =:= (Int, ("b" ->> Boolean) :: ("a" ->> String) :: HNil)]
    assertEquals(rm(r), (1, ("b" ->> true) :: ("a" ->> "x") :: HNil))
  }

  test("the first field with a matching key and value type is the one updated or modified") {
    type R = ("a" ->> Int) :: ("b" ->> Boolean) :: ("a" ->> String) :: HNil
    val r: R = ("a" ->> 1) :: ("b" ->> true) :: ("a" ->> "x") :: HNil

    val u = Updater[R, "a" ->> String]
    summon[u.Out =:= R]
    assertEquals(u(r, "a" ->> "y"), ("a" ->> 1) :: ("b" ->> true) :: ("a" ->> "y") :: HNil)

    val m = Modifier[R, "a", String, Long]
    summon[m.Out =:= (("a" ->> Int) :: ("b" ->> Boolean) :: ("a" ->> Long) :: HNil)]
    assertEquals(m(r, _.length.toLong), ("a" ->> 1) :: ("b" ->> true) :: ("a" ->> 1L) :: HNil)
  }

  test("a field found before an abstract tail keeps the tail in the result types") {
    def ops[L <: HList](r: ("a" ->> Int) :: ("b" ->> String) :: L) = {
      val s = Selector[("a" ->> Int) :: ("b" ->> String) :: L, "b"]
      summon[s.Out =:= String]
      val u = Updater[("a" ->> Int) :: ("b" ->> String) :: L, "b" ->> String]
      summon[u.Out =:= (("a" ->> Int) :: ("b" ->> String) :: L)]
      val m = Modifier[("a" ->> Int) :: ("b" ->> String) :: L, "b", String, Int]
      summon[m.Out =:= (("a" ->> Int) :: ("b" ->> Int) :: L)]
      val rm = Remover[("a" ->> Int) :: ("b" ->> String) :: L, "b"]
      summon[rm.Out =:= (String, ("a" ->> Int) :: L)]
      (s(r), u(r, "b" ->> "y"), m(r, _.length), rm(r))
    }

    val tail = ("c" ->> true) :: HNil
    assertEquals(
      ops(("a" ->> 1) :: ("b" ->> "x") :: tail),
      (
        "x",
        ("a" ->> 1) :: ("b" ->> "y") :: tail,
        ("a" ->> 1) :: ("b" ->> 1) :: tail,
        ("x", ("a" ->> 1) :: tail),
      ),
    )
  }

  test("aliases for fields and tails are kept in the result types") {
    type A = "a" ->> Int
    type Tail = ("c" ->> Boolean) :: HNil
    type R = A :: ("b" ->> String) :: Tail
    val r: R = ("a" ->> 1) :: ("b" ->> "x") :: ("c" ->> true) :: HNil

    val rmA = Remover[R, "a"]
    summon[rmA.Out =:= (Int, ("b" ->> String) :: Tail)]
    assertEquals(rmA(r), (1, ("b" ->> "x") :: ("c" ->> true) :: HNil))

    val rmC = Remover[R, "c"]
    summon[rmC.Out =:= (Boolean, A :: ("b" ->> String) :: HNil)]
    assertEquals(rmC(r), (true, ("a" ->> 1) :: ("b" ->> "x") :: HNil))

    val m = Modifier[R, "b", String, Int]
    summon[m.Out =:= (A :: ("b" ->> Int) :: Tail)]
    assertEquals(m(r, _.length), ("a" ->> 1) :: ("b" ->> 1) :: ("c" ->> true) :: HNil)
  }

  test("a field whose value is a record keeps its nested labels") {
    type Inner = ("x" ->> Int) :: ("y" ->> String) :: HNil
    type R = ("a" ->> Inner) :: ("b" ->> Int) :: HNil
    val inner: Inner = ("x" ->> 1) :: ("y" ->> "z") :: HNil
    val r: R = ("a" ->> inner) :: ("b" ->> 2) :: HNil

    val s = Selector[R, "a"]
    summon[s.Out =:= Inner]
    assertEquals(s(r), inner)

    val rm = Remover[R, "b"]
    summon[rm.Out =:= (Int, ("a" ->> Inner) :: HNil)]
    assertEquals(rm(r), (2, ("a" ->> inner) :: HNil))

    val m = Modifier[R, "a", Inner, ("x" ->> Int) :: HNil]
    summon[m.Out =:= (("a" ->> (("x" ->> Int) :: HNil)) :: ("b" ->> Int) :: HNil)]
    assertEquals(m(r, i => i.head :: HNil), ("a" ->> (("x" ->> 1) :: HNil)) :: ("b" ->> 2) :: HNil)
  }

  test("a field whose type is an intersection around `K ->> V` resolves, wherever it is") {
    type AS = ("a" ->> Int) & Serializable
    type R = AS :: ("b" ->> String) :: HNil
    val r: R = ("a" ->> 1).asInstanceOf[AS] :: ("b" ->> "x") :: HNil

    val s = Selector[R, "a"]
    summon[s.Out =:= Int]
    assertEquals(s(r), 1)

    val rmA = Remover[R, "a"]
    summon[rmA.Out =:= (Int, ("b" ->> String) :: HNil)]
    assertEquals(rmA(r), (1, ("b" ->> "x") :: HNil))

    val rmB = Remover[R, "b"]
    summon[rmB.Out =:= (String, AS :: HNil)]
    assertEquals(rmB(r)._1, "x")
  }

  test("a long record") {
    type R =
      ("k0" ->> Int) :: ("k1" ->> Int) :: ("k2" ->> Int) :: ("k3" ->> Int) :: ("k4" ->> Int) :: ("k5" ->> Int) ::
      ("k6" ->> Int) :: ("k7" ->> Int) :: ("k8" ->> Int) :: ("k9" ->> Int) :: ("k10" ->> Int) :: ("k11" ->> Int) ::
      ("k12" ->> Int) :: ("k13" ->> Int) :: ("k14" ->> Int) :: ("k15" ->> Int) :: ("k16" ->> Int) :: ("k17" ->> Int) ::
      ("k18" ->> Int) :: ("k19" ->> Int) :: ("k20" ->> Int) :: ("k21" ->> Int) :: ("k22" ->> Int) :: ("k23" ->> Int) ::
      ("k24" ->> Int) :: ("k25" ->> Int) :: ("k26" ->> Int) :: ("k27" ->> Int) :: ("k28" ->> Int) :: ("k29" ->> String) ::
      HNil
    type Init =
      ("k0" ->> Int) :: ("k1" ->> Int) :: ("k2" ->> Int) :: ("k3" ->> Int) :: ("k4" ->> Int) :: ("k5" ->> Int) ::
      ("k6" ->> Int) :: ("k7" ->> Int) :: ("k8" ->> Int) :: ("k9" ->> Int) :: ("k10" ->> Int) :: ("k11" ->> Int) ::
      ("k12" ->> Int) :: ("k13" ->> Int) :: ("k14" ->> Int) :: ("k15" ->> Int) :: ("k16" ->> Int) :: ("k17" ->> Int) ::
      ("k18" ->> Int) :: ("k19" ->> Int) :: ("k20" ->> Int) :: ("k21" ->> Int) :: ("k22" ->> Int) :: ("k23" ->> Int) ::
      ("k24" ->> Int) :: ("k25" ->> Int) :: ("k26" ->> Int) :: ("k27" ->> Int) :: ("k28" ->> Int) :: HNil
    val r = HList.fromArray(Array.tabulate[Any](29)(identity) :+ "last").asInstanceOf[R]

    val s = Selector[R, "k29"]
    summon[s.Out =:= String]
    assertEquals(s(r), "last")

    val rm = Remover[R, "k29"]
    summon[rm.Out =:= (String, Init)]
    assertEquals(rm(r)._2.toArray.toList, List.tabulate[Any](29)(identity))

    val m = Modifier[R, "k29", String, Int]
    summon[m.Out =:= HList.Append[Init, "k29" ->> Int]]
    assertEquals(m(r, _.length).toArray.last, 4: Any)
  }

  test("a missing field is refused") {
    refused("""Selector[ABCD, "e"]""")
    refused("""Remover[ABCD, "e"]""")
    refused("""Modifier[ABCD, "e", Int, Int]""")
    refused("""Selector[HNil, "a"]""")
    abortsWith("""FindField.findFieldInst[ABCD, "e" ->> Any, <:<]""", "Failed to find field")
  }

  test("a lookup that matches no field of a record ending in `Nothing` or an abstract tail is refused cleanly") {
    refused("""Modifier[("a" ->> Int) :: Nothing, "b", Int, Int]""")
    refused("""def f[L <: HList]: Any = Modifier[("b" ->> Int) :: L, "a", Int, Int]""")
    refused("""def f[L <: HList]: Any = FindField.findFieldInst[("b" ->> Int) :: L, "a" ->> Int, =:=]""")
  }

  test("Modifier refuses a field whose value type doesn't match") {
    refused("""Modifier[ABCD, "c", String, Int]""")
    refused("""Modifier[ABCD, "c", Any, Int]""")
    refused("""Modifier[("a" ->> Some[Int]) :: HNil, "a", Option[Int], Int]""")
  }

  test("a removed field can't be selected from what's left") {
    val rest = Remover[ABCD, "b"].apply(abcd)._2
    assertEquals(Selector[rest.type, "c"].apply(rest), 3)
    refused("""Selector[rest.type, "b"]""")
  }
}
