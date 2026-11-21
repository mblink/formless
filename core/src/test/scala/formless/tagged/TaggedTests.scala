package formless
package tagged

import munit.FunSuite

class TaggedTests extends FunSuite {
  test("tag constructs an opaque tag with a recoverable value") {
    val tagged: Int @@ "id" = tag["id"](7)
    assertEquals(tagged.untag, 7)
  }

  test("TranslucentTagged.Of preserves the value as a subtype") {
    val constructor = new TranslucentTagged.Of["name"]
    val tagged: TranslucentTagged[String, "name"] = constructor("Ada")
    val value: String = tagged
    assertEquals(value, "Ada")
  }
}
