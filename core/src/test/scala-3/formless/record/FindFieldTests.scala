package formless
package record

import formless.hlist.{::, HList, HNil}
import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

class FindFieldTests extends FunSuite {
  object intKey extends FieldOf[Int]

  object Opaque {
    opaque type A <: ("a" ->> Int) = "a" ->> Int
  }

  val hnil: HNil = HNil

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

  test("keys of different kinds that look alike are told apart") {
    type R = (1 ->> String) :: ("1" ->> Int) :: intKey.F :: (1L ->> Boolean) :: ('1' ->> Double) :: ("k" ->> Char) :: HNil
    val r: R = (1 ->> "int") :: ("1" ->> 2) :: (intKey ->> 3) :: (1L ->> true) :: ('1' ->> 4.0) :: ("k" ->> 'c') :: HNil

    val int = Selector[R, 1]
    summon[int.Out =:= String]
    assertEquals(int(r), "int")
    val string = Selector[R, "1"]
    summon[string.Out =:= Int]
    assertEquals(string(r), 2)
    val obj = Selector[R, intKey.type]
    summon[obj.Out =:= Int]
    assertEquals(obj(r), 3)
    val long = Selector[R, 1L]
    summon[long.Out =:= Boolean]
    assertEquals(long(r), true)
    val char = Selector[R, '1']
    summon[char.Out =:= Double]
    assertEquals(char(r), 4.0)
    val k = Selector[R, "k"]
    summon[k.Out =:= Char]
    assertEquals(k(r), 'c')

    val rm = Remover[R, "1"]
    summon[rm.Out =:= (Int, (1 ->> String) :: intKey.F :: (1L ->> Boolean) :: ('1' ->> Double) :: ("k" ->> Char) :: HNil)]
    assertEquals(rm(r)._1, 2)

    refused("""Selector[R, 2]""")
    refused("""Selector[R, "2"]""")
    refused("""Selector[R, 1.0]""")
    refused("""LacksKey[R, "1"]""")
    summon[LacksKey[R, "2"]]
  }

  test("a string key is found past fields keyed by type parameters, aliases and unions") {
    type A = "a"
    def f[K](r: (K ->> Int) :: (("x" | "y") ->> Long) :: (A ->> String) :: ("b" ->> Boolean) :: HNil) = {
      val a = Selector[(K ->> Int) :: (("x" | "y") ->> Long) :: (A ->> String) :: ("b" ->> Boolean) :: HNil, "a"]
      summon[a.Out =:= String]
      val b = Selector[(K ->> Int) :: (("x" | "y") ->> Long) :: (A ->> String) :: ("b" ->> Boolean) :: HNil, "b"]
      summon[b.Out =:= Boolean]
      val k = Selector[(K ->> Int) :: (("x" | "y") ->> Long) :: (A ->> String) :: ("b" ->> Boolean) :: HNil, K]
      summon[k.Out =:= Int]
      (a(r), b(r), k(r))
    }
    val xy: ("x" | "y") ->> Long = label["x" | "y"](5L)
    assertEquals(f[Unit](label[Unit](1) :: xy :: label[A]("s") :: ("b" ->> true) :: HNil), ("s", true, 1))
  }

  test("a string key spelled through an alias matches the literal") {
    type A = "a"
    val viaFieldKey = Selector[(A ->> Int) :: HNil, "a"]
    summon[viaFieldKey.Out =:= Int]
    val viaSearchedKey = Selector[("b" ->> String) :: ("a" ->> Int) :: HNil, A]
    summon[viaSearchedKey.Out =:= Int]
    assertEquals(viaSearchedKey(("b" ->> "x") :: ("a" ->> 1) :: HNil), 1)
    refused("""LacksKey[(A ->> Int) :: HNil, "a"]""")
    refused("""LacksKey[("a" ->> Int) :: HNil, A]""")
  }

  test("string keys are compared by value") {
    val escaped = Selector[("a" ->> Int) :: HNil, "\u0061"]
    summon[escaped.Out =:= Int]
    val empty = Selector[("a" ->> Int) :: ("" ->> String) :: HNil, ""]
    summon[empty.Out =:= String]
    assertEquals(empty(("a" ->> 1) :: ("" ->> "e") :: HNil), "e")
    refused("""Selector[("a" ->> Int) :: HNil, "A"]""")
    refused("""Selector[("a" ->> Int) :: HNil, "a "]""")
    refused("""Selector[("a" ->> Int) :: HNil, ""]""")
    refused("""Selector[("ab" ->> Int) :: HNil, "a"]""")
  }

  test("a lookup that fails inside a method without a result type is refused cleanly") {
    refused("""def f[H] = Selector[H :: HNil, "a"]""")
    refused("""def f[H] = Remover[H :: HNil, "a"]""")
    refused("""def f[H] = Modifier[H :: HNil, "a", Int, Int]""")
    refused("""def f[H] = Selector[("b" ->> Int) :: H :: HNil, "a"]""")
    refused("""def f[V] = Selector[("b" ->> V) :: HNil, "a"]""")
    refused("""def f[K] = Selector[("a" ->> Int) :: HNil, K]""")
    refused("""def f[K] = Remover[("a" ->> Int) :: HNil, K]""")
    refused("""class C[H] { def s = Selector[H :: HNil, "a"] }""")
    refused("""def f[L <: HList] = Modifier[("b" ->> Int) :: L, "a", Int, Int]""")
  }

  test("abort messages say what was looked for, even inside a method without a result type") {
    abortsWith("""FindField.findFieldInst[ABCD, "e" ->> Any, <:<]""", "Failed to find field")
    abortsWith("""FindField.findFieldInst[ABCD, "e" ->> Any, <:<]""", "ABCD")
    abortsWith("""def f[H] = FindField.findFieldInst[H :: HNil, "e" ->> Any, <:<]""", "Failed to find field")
    abortsWith("""def f[H] = FindField.findFieldInst[H :: HNil, "e" ->> Any, <:<]""", "\"e\"")
    abortsWith("""def f[K] = FindField.findFieldInst[("a" ->> Int) :: HNil, K ->> Any, <:<]""", "Failed to find field")
    abortsWith("""def f[A] = FindField.findFieldInst[List[A] :: HNil, List[A], =:=]""", "Unexpected type")
  }

  test("absence is proven for a record that lacks the key, and refused when it has it") {
    summon[LacksKey[ABCD, "e"]]
    summon[LacksKey[HNil, "a"]]
    refused("""LacksKey[ABCD, "a"]""")
    refused("""LacksKey[ABCD, "c"]""")
    refused("""LacksKey[ABCD, "d"]""")
  }

  test("Updater appends a field the record lacks") {
    val u = Updater[ABCD, "e" ->> Long]
    summon[u.Out =:= HList.Append[ABCD, "e" ->> Long]]
    assertEquals(u(abcd, "e" ->> 5L), ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: ("d" ->> true) :: ("e" ->> 5L) :: HNil)

    val empty = Updater[HNil, "a" ->> Int]
    summon[empty.Out =:= (("a" ->> Int) :: HNil)]
    assertEquals(empty(HNil, "a" ->> 1), ("a" ->> 1) :: HNil)
  }

  test("a tail that is a subtype of HNil ends the record") {
    summon[LacksKey[("a" ->> Int) :: hnil.type, "b"]]
    summon[LacksKey[("a" ->> Int) :: Nothing, "b"]]
    refused("""LacksKey[("a" ->> Int) :: hnil.type, "a"]""")
    val u = Updater[("a" ->> Int) :: hnil.type, "b" ->> Int]
    assertEquals(u(("a" ->> 1) :: hnil, "b" ->> 2), ("a" ->> 1) :: ("b" ->> 2) :: HNil)
  }

  test("absence is not proven for a record with an abstract tail") {
    refused("""def a[L <: HList] = LacksKey[("b" ->> Int) :: L, "a"]""")
    refused("""def a[L <: HList] = LacksKey[L, "a"]""")
    refused("""def a[L <: HList] = Updater[("b" ->> Int) :: L, "a" ->> Int]""")
    refused("""def a[L <: HList] = Updater[L, "a" ->> Int]""")
  }

  test("absence is not proven for a field that has the key but whose value can't be read") {
    refused("""LacksKey[Opaque.A :: HNil, "a"]""")
    refused("""Updater[Opaque.A :: HNil, Opaque.A]""")
    summon[LacksKey[Opaque.A :: HNil, "b"]]
  }

  test("a plain HList is extended with an element type it lacks, and not with one it has") {
    val u = Updater[Int :: String :: HNil, Boolean]
    summon[u.Out =:= (Int :: String :: Boolean :: HNil)]
    assertEquals(u(1 :: "x" :: HNil, true), 1 :: "x" :: true :: HNil)
    refused("""Updater[Int :: String :: HNil, Int]""")
    refused("""Updater[Int :: String :: HNil, String]""")
  }

  test("absence abort messages say why, even inside a method without a result type") {
    abortsWith("""FieldAbsent.fieldAbsentInst[ABCD, "c" ->> Any, <:<]""", "has field")
    abortsWith("""def f[L <: HList] = FieldAbsent.fieldAbsentInst[("b" ->> Int) :: L, "a" ->> Any, <:<]""", "is not statically known")
    abortsWith("""def f[H] = FieldAbsent.fieldAbsentInst[(H ->> Int) :: HNil, H ->> Any, <:<]""", "has field")
  }

  def selectBounded[H <: ("a" ->> Int)](r: H :: HNil) = Selector[H :: HNil, "a"].apply(r)

  test("a field typed as a type parameter takes its key and value from the parameter's bound") {
    def ops[H <: ("a" ->> Int)](r: H :: ("b" ->> String) :: HNil) = {
      val s = Selector[H :: ("b" ->> String) :: HNil, "a"]
      summon[s.Out =:= Int]
      val rmA = Remover[H :: ("b" ->> String) :: HNil, "a"]
      summon[rmA.Out =:= (Int, ("b" ->> String) :: HNil)]
      val rmB = Remover[H :: ("b" ->> String) :: HNil, "b"]
      summon[rmB.Out =:= (String, H :: HNil)]
      val u = Updater[H :: ("b" ->> String) :: HNil, H]
      summon[u.Out =:= (H :: ("b" ->> String) :: HNil)]
      (s(r), rmA(r), rmB(r)._1)
    }
    assertEquals(ops(("a" ->> 1) :: ("b" ->> "x") :: HNil), (1, (1, ("b" ->> "x") :: HNil), "x"))
    assertEquals(selectBounded(("a" ->> 1) :: HNil), 1)
  }

  test("a bound is followed through other type parameters, aliases and intersections") {
    def chain[H1 <: ("a" ->> Int), H2 <: H1](r: H2 :: HNil) = {
      val s = Selector[H2 :: HNil, "a"]
      summon[s.Out =:= Int]
      s(r)
    }
    assertEquals(chain(("a" ->> 1) :: HNil), 1)

    type A = "a" ->> Int
    def alias[H <: A](r: H :: HNil) = {
      val s = Selector[H :: HNil, "a"]
      summon[s.Out =:= Int]
      s(r)
    }
    assertEquals(alias(("a" ->> 2) :: HNil), 2)

    def intersection[H <: ("a" ->> Int) & Serializable](r: H :: HNil) = {
      val s = Selector[H :: HNil, "a"]
      summon[s.Out =:= Int]
      s(r)
    }
    assertEquals(intersection(("a" ->> 3).asInstanceOf[("a" ->> Int) & Serializable] :: HNil), 3)
  }

  test("a bound's key and value may themselves be type parameters") {
    def value[V, H <: ("a" ->> V)](r: H :: HNil) = {
      val s = Selector[H :: HNil, "a"]
      summon[s.Out =:= V]
      s(r)
    }
    assertEquals(value[String, "a" ->> String](("a" ->> "v") :: HNil), "v")

    def key[K, H <: (K ->> Int)](r: H :: HNil) = {
      val s = Selector[H :: HNil, K]
      summon[s.Out =:= Int]
      s(r)
    }
    assertEquals(key[Unit, Unit ->> Int](label[Unit](4) :: HNil), 4)
  }

  test("a field typed as an abstract type member takes its key and value from the member's bound") {
    trait WithField {
      type H <: ("a" ->> Int)
    }
    def get(w: WithField)(r: w.H :: HNil) = {
      val s = Selector[w.H :: HNil, "a"]
      summon[s.Out =:= Int]
      s(r)
    }
    val w = new WithField { type H = "a" ->> Int }
    assertEquals(get(w)(("a" ->> 1) :: HNil), 1)
  }

  test("a field typed as a class's type parameter takes its key and value from the parameter's bound") {
    class C[H <: ("a" ->> Int)] {
      def get(r: H :: HNil) = {
        val s = Selector[H :: HNil, "a"]
        summon[s.Out =:= Int]
        s(r)
      }
    }
    assertEquals(C["a" ->> Int]().get(("a" ->> 1) :: HNil), 1)
  }

  test("an abstract field whose bound isn't a field with the key is refused") {
    refused("""def f[H <: ("b" ->> Int)] = Selector[H :: HNil, "a"]""")
    refused("""def f[H <: Int] = Selector[H :: HNil, "a"]""")
    refused("""trait W { type H }; def f(w: W) = Selector[w.H :: HNil, "a"]""")
    refused("""trait W { type H <: ("b" ->> Int) }; def f(w: W) = Selector[w.H :: HNil, "a"]""")
  }

  test("an element typed as a type parameter without a field bound is not a field") {
    refused("""def f[A] = FindField.findFieldInst[A :: HNil, A, =:=]""")
    refused("""def f[A] = Updater[A :: HNil, A]""")
  }

  test("Modifier still needs an abstract field to be exactly `K ->> A`") {
    refused("""def f[H <: ("a" ->> Int)] = Modifier[H :: HNil, "a", Int, String]""")
  }

  test("an opaque type is not looked through, even when it's bounded by a field") {
    refused("""Selector[Opaque.A :: HNil, "a"]""")
    refused("""Remover[Opaque.A :: HNil, "a"]""")
  }

  test("a field that could only be in an abstract tail is not found") {
    refused("""def a[L <: HList] = Selector[("b" ->> Int) :: L, "a"]""")
    refused("""def a[L <: HList](r: ("b" ->> Int) :: L) = Selector[("b" ->> Int) :: L, "a"].apply(r)""")
    refused("""def a[L <: HList] = Remover[("b" ->> Int) :: L, "a"]""")
    refused("""def a[L <: HList] = Modifier[("b" ->> Int) :: L, "a", Int, Int]""")
    refused("""def a[L <: HList] = Updater[("b" ->> Int) :: L, "a" ->> Int]""")
    refused("""def a[L <: HList] = Selector[L, "a"]""")
    refused("""def a[L <: HList]: Any = Selector[("b" ->> Int) :: ("c" ->> Int) :: L, "a"]""")
    abortsWith("""def f[L <: HList] = FindField.findFieldInst[("b" ->> Int) :: L, "a" ->> Any, <:<]""", "is not statically known")
    abortsWith("""def f[L <: HList] = FindField.findFieldInst[L, "a" ->> Any, <:<]""", "is not statically known")
  }

  test("a tail bounded by a concrete record is walked through its bound") {
    def ops[L <: ("b" ->> Int) :: HNil](r: ("a" ->> String) :: L) = {
      val s = Selector[("a" ->> String) :: L, "b"]
      summon[s.Out =:= Int]
      val rm = Remover[("a" ->> String) :: L, "b"]
      summon[rm.Out =:= (Int, ("a" ->> String) :: HNil)]
      (s(r), rm(r))
    }
    assertEquals(ops(("a" ->> "x") :: ("b" ->> 1) :: HNil), (1, (1, ("a" ->> "x") :: HNil)))
  }

  test("a field missing from a record ending in `Nothing` is not found") {
    refused("""Selector[("a" ->> Int) :: Nothing, "b"]""")
    refused("""Remover[("a" ->> Int) :: Nothing, "b"]""")
    refused("""Selector[Nothing, "a"]""")
    val s = Selector[("a" ->> Int) :: Nothing, "a"]
    summon[s.Out =:= Int]
  }
}
