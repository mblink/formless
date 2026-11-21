package formless
package tagged

import munit.FunSuite

class OpaqueTaggedTests extends FunSuite {
  test("ValueOf lifts the singleton value into the opaque tag") {
    val tagged = summon[ValueOf[OpaqueTagged[7, "id"]]].value
    val value: Int = tagged.untag
    assertEquals((value, tagged.tag), (7, "id"))
  }
}
