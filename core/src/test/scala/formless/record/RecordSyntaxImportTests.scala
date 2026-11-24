package formless.record.syntaxtest

import formless.hlist._
import formless.record.syntax._
import munit.FunSuite

/** `formless.record.syntax` gives fields and the record syntax without the record type classes' clashing names */
class RecordSyntaxImportTests extends FunSuite {
  test("fields and the record syntax work alongside formless.hlist") {
    val r = ("a" ->> 1) :: ("b" ->> "x") :: HNil
    assertEquals(r.get("a"), 1)
    assertEquals(r.updated("a", 2).get("a"), 2)
    assertEquals((r - "a").get("b"), "x")
    assertEquals(field["c"](true), label["c"](true))
  }

  test("Selector names the HList type class, unambiguously") {
    val l = 1 :: "a" :: HNil
    assertEquals(Selector[Int :: String :: HNil, String].apply(l), "a")
  }

  test("the syntax import converts a Map to a record") {
    type Fields = ("a" ->> Int) :: HNil
    assertEquals(Map("a" -> 1).toRecord[Fields], Some(("a" ->> 1) :: HNil))
  }
}
