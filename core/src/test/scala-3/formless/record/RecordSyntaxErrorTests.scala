package formless
package record

import formless.hlist.{::, HList, HNil}
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

object RecordSyntaxErrorTests {
  val plain: Int :: String :: Boolean :: HNil = 1 :: "a" :: true :: HNil
  val rec: ("a" ->> Int) :: ("b" ->> String) :: HNil = ("a" ->> 1) :: ("b" ->> "x") :: HNil
  val mixed: Int :: ("a" ->> Int) :: HNil = 1 :: ("a" ->> 2) :: HNil
}

/** The record syntax's `apply`, `updateWith` and `replace` on a plain HList explain that they hide the HList methods */
class RecordSyntaxErrorTests extends FunSuite {
  import RecordSyntaxErrorTests.*

  private val explanation = "has no record fields, so this `apply`, `updateWith` or `replace` is the record syntax"

  inline def explains(inline code: String)(using munit.Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.exists(_.contains(explanation)), errs.mkString("\n"))
  }

  test("apply, updateWith and replace on a plain HList explain the shadowing") {
    explains("""plain(0)""")
    explains("""plain.updateWith((s: String) => s.length)""")
    explains("""plain.replace("z")""")
    explains("""HNil.updateWith((s: String) => s.length)""")
    // A union of plain lists, checked arm by arm, including arms that end at different points
    explains("""def f(l: HNil | (Int :: HNil)) = l.replace(1)""")
    explains("""def f(l: (Int :: HNil) | (String :: Boolean :: HNil)) = l(0)""")
    val errs = typeCheckErrors("""rec.replace(2)""").map(_.message)
    assert(errs.exists(_.contains("A record's replace takes a key and a value")), errs.mkString("\n"))
  }

  test("the HList methods are still reachable through HList.toHListOps") {
    assertEquals(HList.toHListOps(plain).apply(0), 1)
    assertEquals(HList.toHListOps(plain).updateWith((s: String) => s.length), 1 :: 1 :: true :: HNil)
  }

  test("records, lists with a field, and abstract records keep the record syntax") {
    assertEquals(rec("a"), 1)
    assertEquals(rec.updateWith("a")(_ + 1), ("a" ->> 2) :: ("b" ->> "x") :: HNil)
    assertEquals(rec.replace("b", "y"), ("a" ->> 1) :: ("b" ->> "y") :: HNil)
    assertEquals(mixed("a"), 2)
    // A union with a record arm keeps the record syntax
    val errsU = typeCheckErrors("""def f(l: HNil | (("a" ->> Int) :: HNil)) = l.replace(1)""").map(_.message)
    assert(errsU.exists(_.contains("A record's replace takes a key and a value")), errsU.mkString("\n"))
    def get[R <: HList](r: R)(using s: Selector[R, "a"]): s.Out = r("a")
    assertEquals(get(rec), 1)
    // A missing key in a record is still reported as a missing key
    val errs = typeCheckErrors("""rec("c")""").map(_.message)
    assert(errs.exists(_.contains("Cannot select field")), errs.mkString("\n"))
  }
}
