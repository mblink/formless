package formless
package record

import formless.hlist.{::, HList, HNil}
import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

class LookupTests extends FunSuite {
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

  test("a field whose type is an intersection of fields takes its value from the fields with the key looked up") {
    type Both = ("a" ->> Any) & ("b" ->> String)
    val r: Both :: HNil = ("b" ->> "text").asInstanceOf[Both] :: HNil

    val sb = Selector[Both :: HNil, "b"]
    summon[sb.Out =:= String]
    assertEquals(sb(r), "text")
    val sa = Selector[Both :: HNil, "a"]
    summon[sa.Out =:= Any]

    val rm = Remover[Both :: HNil, "b"]
    summon[rm.Out =:= (String, HNil)]

    type Twice = ("a" ->> Int) & ("a" ->> Serializable) & ("b" ->> String)
    val st = Selector[Twice :: HNil, "a"]
    summon[st.Out =:= (Int & Serializable)]
  }

  test("the fields after the one updated, modified or removed are shared, not copied") {
    val tail = ("b" ->> "tail") :: ("c" ->> true) :: HNil
    type R = ("z" ->> Long) :: ("a" ->> Int) :: tail.type
    val r: R = ("z" ->> 0L) :: ::[("a" ->> Int), tail.type]("a" ->> 1, tail)

    val (removedA, removed) = Remover[R, "a"].apply(r)
    assertEquals(removedA, 1)
    val removedTail: tail.type = removed.tail
    assert(removedTail eq tail)
    assertEquals(removed, ("z" ->> 0L) :: tail)

    val modified = Modifier[R, "a", Int, Long].apply(r, _.toLong)
    val modifiedTail: tail.type = modified.tail.tail
    assert(modifiedTail eq tail)
    assertEquals(modified, ("z" ->> 0L) :: ("a" ->> 1L) :: tail)

    val updated = Updater[R, "a" ->> Int].apply(r, "a" ->> 2)
    assert(updated.tail.tail eq tail)
    assertEquals(updated, ("z" ->> 0L) :: ("a" ->> 2) :: tail)

    // The last field, so the shared rest is `HNil`
    assertEquals(Remover[ABCD, "d"].apply(abcd), (true, ("a" ->> 1) :: ("b" ->> "two") :: ("c" ->> 3) :: HNil))
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
    abortsWith("""FieldOp.fieldOpInst[ABCD, "e" ->> Any, FieldOp.Select]""", "Failed to find field")
  }

  test("a lookup that matches no field of a record ending in `Nothing` or an abstract tail is refused cleanly") {
    refused("""Modifier[("a" ->> Int) :: Nothing, "b", Int, Int]""")
    refused("""def f[L <: HList]: Any = Modifier[("b" ->> Int) :: L, "a", Int, Int]""")
    refused("""def f[L <: HList]: Any = ReplaceAt.replaceAtInst[("b" ->> Int) :: L, "a" ->> Int, ReplaceAt.Field]""")
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

  test("literal keys of every kind are told apart by type as well as value") {
    // `49 == '1'` and `1 == 1L` hold at runtime, but the literal types differ
    type R = (49 ->> String) :: ('1' ->> Int) :: (true ->> Long) :: (1 ->> Char) :: (1L ->> Double) :: (false ->> Unit) :: HNil
    val r: R = (49 ->> "int") :: ('1' ->> 2) :: (true ->> 3L) :: (1 ->> 'c') :: (1L ->> 4.0) :: (false ->> ()) :: HNil

    val char = Selector[R, '1']
    summon[char.Out =:= Int]
    assertEquals(char(r), 2)
    val int = Selector[R, 49]
    summon[int.Out =:= String]
    assertEquals(int(r), "int")
    val long = Selector[R, 1L]
    summon[long.Out =:= Double]
    assertEquals(long(r), 4.0)
    val one = Selector[R, 1]
    summon[one.Out =:= Char]
    assertEquals(one(r), 'c')
    val f = Selector[R, false]
    summon[f.Out =:= Unit]

    type One = 1
    val alias = Selector[R, One]
    summon[alias.Out =:= Char]
    val u = Updater[R, (One ->> Char)]
    summon[u.Out =:= R]
    assertEquals(u(r, (1 ->> 'd')).tail.tail.tail.head: Any, 'd')

    refused("""Selector[R, 2]""")
    refused("""Selector[R, 49L]""")
    refused("""LacksKey[R, 1]""")
    summon[LacksKey[R, 2]]
    summon[LacksKey[R, 49L]]
    summon[LacksKey[R, '2']]
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
    abortsWith("""FieldOp.fieldOpInst[ABCD, "e" ->> Any, FieldOp.Select]""", "Failed to find field")
    abortsWith("""FieldOp.fieldOpInst[ABCD, "e" ->> Any, FieldOp.Select]""", "ABCD")
    abortsWith("""def f[H] = FieldOp.fieldOpInst[H :: HNil, "e" ->> Any, FieldOp.Select]""", "Failed to find field")
    abortsWith("""def f[H] = FieldOp.fieldOpInst[H :: HNil, "e" ->> Any, FieldOp.Select]""", "\"e\"")
    abortsWith("""def f[K] = FieldOp.fieldOpInst[("a" ->> Int) :: HNil, K ->> Any, FieldOp.Select]""", "Failed to find field")
    abortsWith("""def f[A] = ReplaceAt.replaceAtInst[List[A] :: HNil, List[A], ReplaceAt.Field]""", "isn't a field")
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

  test("absence is not proven for a record type whose bound refers to itself, rather than walked forever") {
    refused("""trait Cyclic { type L <: Int :: L }; def f(c: Cyclic) = LacksKey[c.L, "a"]""")
  }

  test("ambiguous comparison evidence still counts as a match") {
    refused("""def f(using p: ("a" ->> Int) <:< ("a" ->> Any), q: ("a" ->> Int) <:< ("a" ->> Any)) = LacksKey[("a" ->> Int) :: HNil, "a"]""")
  }

  test("absence from a union of records is proven arm by arm") {
    type A = "a" ->> Int
    type B = "b" ->> String
    summon[LacksKey[(A :: HNil) | (A :: B :: HNil), "c"]]
    refused("""LacksKey[(A :: HNil) | (A :: B :: HNil), "b"]""")
    refused("""LacksKey[HNil | (A :: HNil), "a"]""")
    summon[LacksKey[HNil | (A :: HNil), "c"]]
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

  test("an abstract type member's bound is seen from its prefix") {
    trait WithField[A] {
      type H <: ("a" ->> A)
    }
    def get(w: WithField[Int])(r: w.H :: HNil): Int = {
      val s = Selector[w.H :: HNil, "a"]
      summon[s.Out =:= Int]
      s(r)
    }
    val w = new WithField[Int] { type H = "a" ->> Int }
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
    refused("""def f[A] = ReplaceAt.replaceAtInst[A :: HNil, A, ReplaceAt.Field]""")
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
    abortsWith("""def f[L <: HList] = FieldOp.fieldOpInst[("b" ->> Int) :: L, "a" ->> Any, FieldOp.Select]""", "is not statically known")
    abortsWith("""def f[L <: HList] = FieldOp.fieldOpInst[L, "a" ->> Any, FieldOp.Select]""", "is not statically known")
  }

  test("a record type whose bound refers to itself is refused rather than walked forever") {
    refused("""trait Cyclic { type L <: ("b" ->> Int) :: L }; def f(c: Cyclic) = Selector[c.L, "a"]""")
  }

  test("a union of records is walked arm by arm") {
    type A = "a" ->> Int
    type B = "b" ->> String
    type R = (A :: HNil) | (A :: B :: HNil)
    val r: R = ("a" ->> 1) :: ("b" ->> "x") :: HNil
    val s = Selector[R, "a"]
    summon[s.Out =:= Int]
    assertEquals(s(r), 1)
    // A field typed as a union of fields with the same key
    val su = Selector[(A | ("a" ->> String)) :: HNil, "a"]
    summon[su.Out =:= (Int | String)]
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

  test("absence is not proven when an element might match once its type parameters are known") {
    refused("""def f[H] = LacksKey[H :: HNil, "a"]""")
    refused("""def f[K] = LacksKey[(K ->> Int) :: HNil, "a"]""")
    refused("""def f[K] = LacksKey[("a" ->> Int) :: HNil, K]""")
    refused("""def f[H] = Updater[H :: HNil, "a" ->> Int]""")
    refused("""def f[V] = Updater[("a" ->> V) :: HNil, "a" ->> Int]""")
    abortsWith("""def f[H] = FieldAbsent.fieldAbsentInst[H :: HNil, "a" ->> Any, <:<]""", "might match")
  }

  test("absence is proven past elements that can't match whatever their type parameters are") {
    assertEquals(typeCheckErrors("""def f[V] = LacksKey[("a" ->> V) :: HNil, "b"]""").map(_.message), Nil)
    assertEquals(
      typeCheckErrors("""def f[H <: ("a" ->> Int)] = { val u = Updater[H :: HNil, "b" ->> Int]; summon[u.Out =:= (H :: ("b" ->> Int) :: HNil)] }""")
        .map(_.message),
      Nil,
    )
    assertEquals(
      typeCheckErrors("""def f[V] = { val u = Updater[("a" ->> V) :: HNil, "b" ->> Int]; summon[u.Out =:= (("a" ->> V) :: ("b" ->> Int) :: HNil)] }""")
        .map(_.message),
      Nil,
    )
    val u = Updater[("a" ->> String) :: HNil, "a" ->> Int]
    summon[u.Out =:= (("a" ->> String) :: ("a" ->> Int) :: HNil)]
  }

  test("absence is not proven for an abstract field bounded by another key, which may have both keys") {
    // `H` may be `("a" ->> Int) & ("b" ->> Int)`, for which absence of `"b"` isn't proven either
    refused("""def f[H <: ("a" ->> Int)] = LacksKey[H :: HNil, "b"]""")
    refused("""LacksKey[(("a" ->> Int) & ("b" ->> Int)) :: HNil, "b"]""")
  }

  test("absence is not proven for an element that is a singleton of an abstract type") {
    refused("""def f[T](x: T) = LacksKey[x.type :: HNil, "b"]""")
    // `b.field.type` widens to `Any` here, but is a subtype of `"a" ->> Int` once `B` is `BoxA.type`
    refused("""trait Box { val field: Any }; def f[B <: Box](b: B) = LacksKey[b.field.type :: HNil, "a"]""")
    assertEquals(typeCheckErrors("""val x = 1; LacksKey[x.type :: HNil, "b"]""").map(_.message), Nil)
  }

  test("absence is not proven from an abstract record's bound, whose elements may be narrower") {
    // `L` may be `("a" ->> Int) :: HNil`, which already has the field
    refused("""def f[L <: ("a" ->> Any) :: HNil] = Updater[L, "a" ->> Int]""")
    refused("""def f[L <: ("a" ->> Any) :: HNil] = LacksKey[L, "b"]""")
    // Under `=:=`, a bound's key still rules out other keys
    assertEquals(
      typeCheckErrors("""def f[L <: ("a" ->> Any) :: HNil] = { val u = Updater[L, "b" ->> Int]; summon[u.Out =:= HList.Append[L, "b" ->> Int]] }""")
        .map(_.message),
      Nil,
    )
    // A field found through the bound still resolves
    assertEquals(typeCheckErrors("""def f[L <: ("a" ->> Int) :: HNil] = Selector[L, "a"]""").map(_.message), Nil)
  }

  test("a field whose value has a method refinement is not mistaken for an abstract one") {
    type Payload = AnyRef { def value(i: Int): String }
    val u = Updater[("a" ->> Payload) :: HNil, "a" ->> Int]
    summon[u.Out =:= (("a" ->> Payload) :: ("a" ->> Int) :: HNil)]
    summon[LacksKey[("a" ->> Payload) :: HNil, "b"]]
  }

  test("Updater says why it neither replaces nor appends") {
    abortsWith("""def f[V] = FieldUpdate.fieldUpdateInst[("a" ->> V) :: HNil, "a" ->> Int]""", "might match")
    abortsWith("""def f[L <: HList] = FieldUpdate.fieldUpdateInst[("b" ->> Int) :: L, "a" ->> Int]""", "is not statically known")
    abortsWith("""FieldUpdate.fieldUpdateInst[Int :: String :: HNil, Int]""", "isn't a field")
    // A field found in only one arm of a union of records is neither replaced nor proven absent
    refused("""Updater[HNil | (("a" ->> Int) :: HNil), "a" ->> Int]""")
    val u = Updater[HNil | (("a" ->> Int) :: HNil), "b" ->> Int]
    summon[u.Out =:= ((("b" ->> Int) :: HNil) | (("a" ->> Int) :: ("b" ->> Int) :: HNil))]
  }

  test("absence is not proven for keys that are method parameters' singletons, which may be the same path") {
    refused("""def lacks(x: String, y: String): LacksKey[(x.type ->> Int) :: HNil, y.type] = summon""")
    summon[LacksKey[(intKey.type ->> Int) :: HNil, "b"]]
  }

  test("absence is not proven for an opaque type whose bound is abstract") {
    refused("""class Box[T] { object O { opaque type X <: T = T }; def lacks = LacksKey[O.X :: HNil, "a"] }""")
    // A bound that is known still decides it
    summon[LacksKey[Opaque.A :: HNil, "b"]]
  }

  test("a field whose value has a type lambda in it is not mistaken for an abstract one") {
    trait Payload[F[_]]
    type P = Payload[[X] =>> Either[Int, X]]
    val u = Updater[("a" ->> P) :: HNil, "a" ->> Int]
    summon[u.Out =:= (("a" ->> P) :: ("a" ->> Int) :: HNil)]
    summon[LacksKey[("a" ->> P) :: HNil, "b"]]
  }

  test("a field that is an intersection with an aliased field in it is replaced, not refused") {
    val u = Updater[ASAlias.Inter, ASAlias.AS]
    summon[u.Out =:= ASAlias.Inter]
    assertEquals(u(ASAlias.inter, ("a" ->> 2).asInstanceOf[ASAlias.AS]), ("a" ->> 2) :: ("b" ->> "x") :: HNil)
  }

  test("absence is not proven for a union element when any of its arms might match") {
    type A = "a" ->> Int
    type B = "b" ->> Int
    refused("""LacksKey[(A :: HNil) | (B :: HNil), "a"]""")
    refused("""LacksKey[(A | B) :: HNil, "a"]""")
    refused("""Updater[(A :: HNil) | (B :: HNil), A]""")
    refused("""formless.hlist.NotContains[(Int :: HNil) | (String :: HNil), Int]""")
    // Arms that can't match still let absence be proven
    summon[LacksKey[(A :: HNil) | (B :: HNil), "c"]]
    summon[LacksKey[(A | B) :: HNil, "c"]]
    val u = Updater[(A :: HNil) | (B :: HNil), "c" ->> Int]
    summon[u.Out =:= ((A :: ("c" ->> Int) :: HNil) | (B :: ("c" ->> Int) :: HNil))]
    assertEquals(u(("a" ->> 1) :: HNil, "c" ->> 2), ("a" ->> 1) :: ("c" ->> 2) :: HNil)
  }

  test("unions of long records are walked within the budget") {
    // Split across aliases: one infix type of 250 elements can overflow the compiler's stack while it is typed
    type R200 = ("k200" ->> Int) :: ("k201" ->> Int) :: ("k202" ->> Int) :: ("k203" ->> Int) :: ("k204" ->> Int) :: ("k205" ->> Int) :: ("k206" ->> Int) :: ("k207" ->> Int) :: ("k208" ->> Int) :: ("k209" ->> Int) :: ("k210" ->> Int) :: ("k211" ->> Int) :: ("k212" ->> Int) :: ("k213" ->> Int) :: ("k214" ->> Int) :: ("k215" ->> Int) :: ("k216" ->> Int) :: ("k217" ->> Int) :: ("k218" ->> Int) :: ("k219" ->> Int) :: ("k220" ->> Int) :: ("k221" ->> Int) :: ("k222" ->> Int) :: ("k223" ->> Int) :: ("k224" ->> Int) :: ("k225" ->> Int) :: ("k226" ->> Int) :: ("k227" ->> Int) :: ("k228" ->> Int) :: ("k229" ->> Int) :: ("k230" ->> Int) :: ("k231" ->> Int) :: ("k232" ->> Int) :: ("k233" ->> Int) :: ("k234" ->> Int) :: ("k235" ->> Int) :: ("k236" ->> Int) :: ("k237" ->> Int) :: ("k238" ->> Int) :: ("k239" ->> Int) :: ("k240" ->> Int) :: ("k241" ->> Int) :: ("k242" ->> Int) :: ("k243" ->> Int) :: ("k244" ->> Int) :: ("k245" ->> Int) :: ("k246" ->> Int) :: ("k247" ->> Int) :: ("k248" ->> Int) :: ("k249" ->> Int) :: HNil
    type R150 = ("k150" ->> Int) :: ("k151" ->> Int) :: ("k152" ->> Int) :: ("k153" ->> Int) :: ("k154" ->> Int) :: ("k155" ->> Int) :: ("k156" ->> Int) :: ("k157" ->> Int) :: ("k158" ->> Int) :: ("k159" ->> Int) :: ("k160" ->> Int) :: ("k161" ->> Int) :: ("k162" ->> Int) :: ("k163" ->> Int) :: ("k164" ->> Int) :: ("k165" ->> Int) :: ("k166" ->> Int) :: ("k167" ->> Int) :: ("k168" ->> Int) :: ("k169" ->> Int) :: ("k170" ->> Int) :: ("k171" ->> Int) :: ("k172" ->> Int) :: ("k173" ->> Int) :: ("k174" ->> Int) :: ("k175" ->> Int) :: ("k176" ->> Int) :: ("k177" ->> Int) :: ("k178" ->> Int) :: ("k179" ->> Int) :: ("k180" ->> Int) :: ("k181" ->> Int) :: ("k182" ->> Int) :: ("k183" ->> Int) :: ("k184" ->> Int) :: ("k185" ->> Int) :: ("k186" ->> Int) :: ("k187" ->> Int) :: ("k188" ->> Int) :: ("k189" ->> Int) :: ("k190" ->> Int) :: ("k191" ->> Int) :: ("k192" ->> Int) :: ("k193" ->> Int) :: ("k194" ->> Int) :: ("k195" ->> Int) :: ("k196" ->> Int) :: ("k197" ->> Int) :: ("k198" ->> Int) :: ("k199" ->> Int) :: R200
    type R100 = ("k100" ->> Int) :: ("k101" ->> Int) :: ("k102" ->> Int) :: ("k103" ->> Int) :: ("k104" ->> Int) :: ("k105" ->> Int) :: ("k106" ->> Int) :: ("k107" ->> Int) :: ("k108" ->> Int) :: ("k109" ->> Int) :: ("k110" ->> Int) :: ("k111" ->> Int) :: ("k112" ->> Int) :: ("k113" ->> Int) :: ("k114" ->> Int) :: ("k115" ->> Int) :: ("k116" ->> Int) :: ("k117" ->> Int) :: ("k118" ->> Int) :: ("k119" ->> Int) :: ("k120" ->> Int) :: ("k121" ->> Int) :: ("k122" ->> Int) :: ("k123" ->> Int) :: ("k124" ->> Int) :: ("k125" ->> Int) :: ("k126" ->> Int) :: ("k127" ->> Int) :: ("k128" ->> Int) :: ("k129" ->> Int) :: ("k130" ->> Int) :: ("k131" ->> Int) :: ("k132" ->> Int) :: ("k133" ->> Int) :: ("k134" ->> Int) :: ("k135" ->> Int) :: ("k136" ->> Int) :: ("k137" ->> Int) :: ("k138" ->> Int) :: ("k139" ->> Int) :: ("k140" ->> Int) :: ("k141" ->> Int) :: ("k142" ->> Int) :: ("k143" ->> Int) :: ("k144" ->> Int) :: ("k145" ->> Int) :: ("k146" ->> Int) :: ("k147" ->> Int) :: ("k148" ->> Int) :: ("k149" ->> Int) :: R150
    type R50 = ("k50" ->> Int) :: ("k51" ->> Int) :: ("k52" ->> Int) :: ("k53" ->> Int) :: ("k54" ->> Int) :: ("k55" ->> Int) :: ("k56" ->> Int) :: ("k57" ->> Int) :: ("k58" ->> Int) :: ("k59" ->> Int) :: ("k60" ->> Int) :: ("k61" ->> Int) :: ("k62" ->> Int) :: ("k63" ->> Int) :: ("k64" ->> Int) :: ("k65" ->> Int) :: ("k66" ->> Int) :: ("k67" ->> Int) :: ("k68" ->> Int) :: ("k69" ->> Int) :: ("k70" ->> Int) :: ("k71" ->> Int) :: ("k72" ->> Int) :: ("k73" ->> Int) :: ("k74" ->> Int) :: ("k75" ->> Int) :: ("k76" ->> Int) :: ("k77" ->> Int) :: ("k78" ->> Int) :: ("k79" ->> Int) :: ("k80" ->> Int) :: ("k81" ->> Int) :: ("k82" ->> Int) :: ("k83" ->> Int) :: ("k84" ->> Int) :: ("k85" ->> Int) :: ("k86" ->> Int) :: ("k87" ->> Int) :: ("k88" ->> Int) :: ("k89" ->> Int) :: ("k90" ->> Int) :: ("k91" ->> Int) :: ("k92" ->> Int) :: ("k93" ->> Int) :: ("k94" ->> Int) :: ("k95" ->> Int) :: ("k96" ->> Int) :: ("k97" ->> Int) :: ("k98" ->> Int) :: ("k99" ->> Int) :: R100
    type R = ("k0" ->> Int) :: ("k1" ->> Int) :: ("k2" ->> Int) :: ("k3" ->> Int) :: ("k4" ->> Int) :: ("k5" ->> Int) :: ("k6" ->> Int) :: ("k7" ->> Int) :: ("k8" ->> Int) :: ("k9" ->> Int) :: ("k10" ->> Int) :: ("k11" ->> Int) :: ("k12" ->> Int) :: ("k13" ->> Int) :: ("k14" ->> Int) :: ("k15" ->> Int) :: ("k16" ->> Int) :: ("k17" ->> Int) :: ("k18" ->> Int) :: ("k19" ->> Int) :: ("k20" ->> Int) :: ("k21" ->> Int) :: ("k22" ->> Int) :: ("k23" ->> Int) :: ("k24" ->> Int) :: ("k25" ->> Int) :: ("k26" ->> Int) :: ("k27" ->> Int) :: ("k28" ->> Int) :: ("k29" ->> Int) :: ("k30" ->> Int) :: ("k31" ->> Int) :: ("k32" ->> Int) :: ("k33" ->> Int) :: ("k34" ->> Int) :: ("k35" ->> Int) :: ("k36" ->> Int) :: ("k37" ->> Int) :: ("k38" ->> Int) :: ("k39" ->> Int) :: ("k40" ->> Int) :: ("k41" ->> Int) :: ("k42" ->> Int) :: ("k43" ->> Int) :: ("k44" ->> Int) :: ("k45" ->> Int) :: ("k46" ->> Int) :: ("k47" ->> Int) :: ("k48" ->> Int) :: ("k49" ->> Int) :: R50
    val s = Selector[R | R, "k249"]
    summon[s.Out =:= Int]
  }

  type MsgR = ("a" ->> Int) :: ("b" ->> String) :: HNil
  val msgR: MsgR = ("a" ->> 1) :: ("b" ->> "x") :: HNil

  test("a failed lookup names the record and the key, through the syntax as well as the type classes") {
    abortsWith("""Selector[MsgR, "c"]""", "Cannot select field (\"c\" : String) from record")
    abortsWith("""msgR.get("c")""", "Cannot select field (\"c\" : String) from record")
    abortsWith("""msgR.replace("c", 1)""", "Cannot select field (\"c\" : String) from record")
    abortsWith("""msgR - "c"""", "Cannot remove field (\"c\" : String) from record")
    abortsWith("""Remover[MsgR, "c"]""", "Cannot remove field (\"c\" : String) from record")
    abortsWith("""Modifier[MsgR, "a", String, Int]""", "Cannot modify field (\"a\" : String) of record")
    abortsWith("""LacksKey[MsgR, "a"]""", "Cannot prove that record")
    abortsWith("""def f[T <: HList] = Updater[("b" ->> String) :: T, "a" ->> Int]""", "Cannot update record")
    abortsWith("""def f[T <: HList](t: ("b" ->> String) :: T) = t.get("a")""", "abstract part")
  }

  test("Modifier and Replacer leave the new value's type to be inferred from the expected result type") {
    // `Tab(None, 1)` alone would infer `Tab[Nothing]`; the expected record type makes it `Tab[Unit]`, as in 0.8.x
    type R = ("all" ->> ModTab[Unit]) :: ("n" ->> Int) :: HNil
    val r: R = ("all" ->> ModTab[Unit](Some(()), 0)) :: ("n" ->> 1) :: HNil
    val updated: R = r.updateWith("all")(_ => ModTab(None, 1))
    assertEquals(updated, ("all" ->> ModTab[Unit](None, 1)) :: ("n" ->> 1) :: HNil)

    type L = Int :: ModTab[Unit] :: HNil
    val l: L = 1 :: ModTab[Unit](Some(()), 0) :: HNil
    val modified: L = formless.hlist.HList.toHListOps(l).updateWith((_: ModTab[Unit]) => ModTab(None, 2))
    assertEquals(modified, 1 :: ModTab[Unit](None, 2) :: HNil)
    val replaced: (ModTab[Unit], L) = formless.hlist.HList.toHListOps(l).replaceType[ModTab[Unit]](ModTab(None, 3))
    assertEquals(replaced._2, 1 :: ModTab[Unit](None, 3) :: HNil)
  }
}

final case class ModTab[A](a: Option[A], n: Int)

object ASAlias {
  type A = "a" ->> Int
  type AS = A & Serializable
  type Inter = AS :: ("b" ->> String) :: HNil
  val inter: Inter = ("a" ->> 1).asInstanceOf[AS] :: ("b" ->> "x") :: HNil
}
