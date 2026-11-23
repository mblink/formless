package formless
package tagged

import munit.FunSuite

class OpaqueTaggedTests extends FunSuite {
  test("ValueOf lifts the singleton value into the opaque tag") {
    val tagged = summon[ValueOf[OpaqueTagged[7, "id"]]].value
    val value: Int = tagged.untag
    assertEquals((value, tagged.tag), (7, "id"))
  }

  test("the tag constructor can tag and untag a value inside a type constructor") {
    val id = OpaqueTagged["id"]
    val tagged: List[OpaqueTagged[Int, "id"]] = id.subst(List(7))
    assertEquals(tagged.map(id.untag), List(7))
    assertEquals(id.tag, "id")
  }

  test("mapping changes the underlying value and preserves the tag") {
    val tagged: OpaqueTagged[String, "id"] = OpaqueTagged["id"](7).map(_.toString)
    assertEquals((tagged.untag, tagged.tag), ("7", "id"))
  }

  test("deriving reuses the underlying type class instance") {
    val ordering = OpaqueTagged.deriving[Ordering, Int, "id"]
    val id = OpaqueTagged["id"]
    assert(ordering.lt(id(1), id(2)))
  }

  trait Lift[F[_]] {
    def apply[A](a: A): F[A]
  }

  test("derivingK transports a higher-kinded instance to an opaque tag") {
    given Lift[[a] =>> a] with {
      def apply[A](a: A): A = a
    }
    val lift = OpaqueTagged.derivingK[Lift, "id"]
    val tagged: OpaqueTagged[Int, "id"] = lift(7)
    assertEquals(tagged.untag, 7)
  }

  test("a translucent tag exposes the value of its singleton tag") {
    import TranslucentTagged.{label, tag}
    val tagged = TranslucentTagged["id"](7)
    assertEquals((tagged.tag, tagged.label), ("id", "id"))
  }
}
