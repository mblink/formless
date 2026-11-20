package formless
package hlist

import munit.{FunSuite, Location}
import scala.compiletime.testing.typeCheckErrors

class ElemOpTests extends FunSuite {
  // A crash during macro expansion also fails to compile, so only a clean refusal passes
  inline def refused(inline code: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.nonEmpty, s"expected a compile error: $code")
    assert(!errs.exists(_.contains("Exception occurred while executing macro expansion")), errs.mkString("\n"))
  }

  inline def compiles(inline code: String)(using loc: Location): Unit =
    assertEquals(typeCheckErrors(code).map(_.message), Nil)

  type L = Int :: String :: Boolean :: String :: Double :: HNil
  val l: L = 1 :: "a" :: true :: "b" :: 2.0 :: HNil

  test("Remove takes out the first element of the type, and reinsert puts it back") {
    val r = Remove[L, String]
    summon[r.Out =:= (String, Int :: Boolean :: String :: Double :: HNil)]
    assertEquals(r(l), ("a", 1 :: true :: "b" :: 2.0 :: HNil))
    assertEquals(r.reinsert(r(l)), l)

    val last = Remove[L, Double]
    summon[last.Out =:= (Double, Int :: String :: Boolean :: String :: HNil)]
    assertEquals(last.reinsert(last(l)), l)
  }

  test("Replacer and Modifier replace the first element of the type") {
    val r = Replacer[L, String, Char]
    summon[r.Out =:= (String, Int :: Char :: Boolean :: String :: Double :: HNil)]
    assertEquals(r(l, 'c'), ("a", 1 :: 'c' :: true :: "b" :: 2.0 :: HNil))

    val m = Modifier[L, String, Int]
    summon[m.Out =:= (String, Int :: Int :: Boolean :: String :: Double :: HNil)]
    assertEquals(m(l, _.length), ("a", 1 :: 1 :: true :: "b" :: 2.0 :: HNil))
  }

  test("left splits are at the first element of the type") {
    val s = SplitLeft[L, String]
    summon[s.Prefix =:= (Int :: HNil)]
    summon[s.Suffix =:= (String :: Boolean :: String :: Double :: HNil)]
    assertEquals(s(l), (1 :: HNil, "a" :: true :: "b" :: 2.0 :: HNil))

    val r = ReverseSplitLeft[L, Boolean]
    summon[r.Prefix =:= (String :: Int :: HNil)]
    summon[r.Suffix =:= (Boolean :: String :: Double :: HNil)]
    assertEquals(r(l), ("a" :: 1 :: HNil, true :: "b" :: 2.0 :: HNil))

    val first = SplitLeft[L, Int]
    summon[first.Prefix =:= HNil]
    assertEquals(first(l), (HNil, l))
  }

  test("right splits are at the last element of the type") {
    val s = SplitRight[L, String]
    summon[s.Prefix =:= (Int :: String :: Boolean :: String :: HNil)]
    summon[s.Suffix =:= (Double :: HNil)]
    assertEquals(s(l), (1 :: "a" :: true :: "b" :: HNil, 2.0 :: HNil))

    val r = ReverseSplitRight[L, String]
    summon[r.Prefix =:= (String :: Boolean :: String :: Int :: HNil)]
    summon[r.Suffix =:= (Double :: HNil)]
    assertEquals(r(l), ("b" :: true :: "a" :: 1 :: HNil, 2.0 :: HNil))

    val last = SplitRight[L, Double]
    summon[last.Suffix =:= HNil]
    assertEquals(last(l), (l, HNil))
  }

  test("the result types equal the match types the operations used to be defined with") {
    val sl = SplitLeft[L, Boolean]
    summon[sl.Prefix =:= HList.Take[L, 2]]
    summon[sl.Suffix =:= HList.Drop[L, 2]]
    val rsl = ReverseSplitLeft[L, Boolean]
    summon[rsl.Prefix =:= HList.Reverse[HList.Take[L, 2]]]
    val sr = SplitRight[L, String]
    summon[sr.Prefix =:= HList.Take[L, 4]]
    summon[sr.Suffix =:= HList.Drop[L, 4]]
    val rsr = ReverseSplitRight[L, String]
    summon[rsr.Prefix =:= HList.Reverse[HList.Take[L, 4]]]
    val rm = Remove[L, String]
    summon[rm.Out =:= (String, Int :: HList.Drop[L, 2])]
  }

  test("a missing element is refused") {
    refused("""Remove[L, Char]""")
    refused("""SplitLeft[L, Char]""")
    refused("""SplitRight[L, Char]""")
    refused("""Replacer[L, Char, Int]""")
    refused("""Modifier[HNil, Int, Int]""")
  }

  test("an element found before an abstract tail keeps the tail in the result types") {
    def remove[T <: HList](t: String :: T) = {
      val r = Remove[String :: T, String]
      summon[r.Out =:= (String, T)]
      r(t)
    }
    assertEquals(remove("a" :: 1 :: HNil), ("a", 1 :: HNil))

    def split[T <: HList](t: Int :: String :: T) = {
      val s = SplitLeft[Int :: String :: T, String]
      summon[s.Suffix =:= (String :: T)]
      s(t)
    }
    assertEquals(split(1 :: "a" :: true :: HNil), (1 :: HNil, "a" :: true :: HNil))
  }

  test("an element that could only be in an abstract tail, or a `Nothing` one, is refused") {
    refused("""def f[T <: HList] = Remove[Int :: T, String]""")
    refused("""def f[T <: HList] = Replacer[Int :: T, String, Char]""")
    refused("""Remove[Int :: Nothing, String]""")
    refused("""Remove[Nothing, String]""")
    // A later match could be in the tail, so the last isn't known
    refused("""def f[T <: HList] = SplitRight[String :: T, String]""")
    refused("""def f[T <: HList] = ReverseSplitRight[String :: T, String]""")
    // `T` may be `Int :: HNil`, whose `Int` would be the last
    refused("""def f[T <: Any :: HNil] = SplitRight[Int :: T, Int]""")
    refused("""def f[T <: Int :: Any :: HNil] = SplitRight[T, Int]""")
    // A later match past an element that might match is still the last
    compiles("""def f[H] = { val s = SplitRight[Int :: H :: Int :: HNil, Int]; summon[s.Suffix =:= HNil] }""")
  }

  test("a list type whose bound refers to itself is refused rather than walked forever") {
    refused("""trait C { type L <: Int :: L }; def f(c: C) = Remove[c.L, String]""")
    refused("""trait C { type L <: Int :: L }; def f(c: C) = SplitRight[c.L, Int]""")
    refused("""trait C { type L <: Int :: String :: L }; def f(c: C) = SplitRight[c.L, Int]""")
    // Its type argument changes at each step, so the types differ but the abstract type they stand for doesn't
    refused("""trait C { type L[A] <: Int :: L[List[A]] }; def f(c: C) = SplitRight[c.L[Int], Int]""")
    refused("""trait C { type L[A] <: Int :: L[List[A]] }; def f(c: C) = Remove[c.L[Int], String]""")
  }

  test("NotContains is proven only when no element could be the type") {
    summon[NotContains[L, Char]]
    summon[NotContains[HNil, Char]]
    refused("""NotContains[L, String]""")
    refused("""def f[T <: HList] = NotContains[Int :: T, String]""")
    refused("""def f[H] = NotContains[H :: HNil, Int]""")
    compiles("""def f[T <: HList] = NotContains[Int :: String :: HNil, Char]""")
  }

  test("the elements after the one changed, removed or split at are shared, not copied") {
    val tail = true :: 2.0 :: HNil
    val t = 1 :: ::[String, tail.type]("a", tail)

    assert(Remove[Int :: String :: tail.type, String].apply(t)._2.tail eq tail)
    assert(Replacer[Int :: String :: tail.type, String, Char].apply(t, 'c')._2.tail.tail eq tail)
    assert(SplitLeft[Int :: String :: tail.type, String].apply(t)._2.tail eq tail)
    assert(SplitRight[Int :: String :: tail.type, String].apply(t)._2 eq tail)

    // The suffix of a left split keeps the singleton type it's spelled with
    val lt = 1 :: ::[String, tail.type]("a", tail)
    val sl = SplitLeft[Int :: String :: tail.type, Boolean]
    val sameTail: tail.type = sl(lt)._2
    assert(sameTail eq tail)
    val rsl = ReverseSplitLeft[Int :: String :: tail.type, Boolean]
    val sameTail2: tail.type = rsl(lt)._2
    assert(sameTail2 eq tail)
  }

  test("a long list") {
    type L30 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 ::
      20 :: 21 :: 22 :: 23 :: 24 :: 25 :: 26 :: 27 :: 28 :: 29 :: HNil
    // Built at runtime rather than with `constValueHList`, whose inline expansion over 30 elements can overflow the
    // compiler's stack
    val l30: L30 = (0 until 30).foldRight(HNil: HList)((i, t) => ::(i, t)).asInstanceOf[L30]
    val s = SplitLeft[L30, 17]
    summon[s.Suffix =:= HList.Drop[L30, 17]]
    assertEquals(s(l30)._2.head, 17)
    val r = Remove[L30, 29]
    assertEquals(r(l30)._1, 29)
    assertEquals(r.reinsert(r(l30)), l30)
  }
}
