package formless

import formless.hlist.{::, HNil}
import formless.record._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import munit.FunSuite

/** Type class instances are `Serializable`, so they can be captured by closures that are, e.g. in Spark jobs */
class SerializationTests extends FunSuite {
  def roundTrip[A <: AnyRef](a: A): A = {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(a)
    out.close()
    new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray)).readObject().asInstanceOf[A]
  }

  type R = ("a" ->> Int) :: ("b" ->> String) :: HNil
  val r: R = ("a" ->> 1) :: ("b" ->> "x") :: HNil

  test("a record Updater that appends survives serialization") {
    val u = roundTrip(Updater[HNil, "a" ->> Int])
    assertEquals(u(HNil, "a" ->> 2), ("a" ->> 2) :: HNil)
  }

  test("a record Updater that replaces survives serialization") {
    val u = roundTrip(Updater[R, "b" ->> String])
    assertEquals(u(r, "b" ->> "y"), ("a" ->> 1) :: ("b" ->> "y") :: HNil)
  }

  test("record Selector, Remover and Modifier survive serialization") {
    assertEquals(roundTrip(Selector[R, "b"]).apply(r), "x")
    assertEquals(roundTrip(Remover[R, "a"]).apply(r), (1, ("b" ->> "x") :: HNil))
    assertEquals(roundTrip(Modifier[R, "a", Int, Int]).apply(r, (_: Int) + 1), ("a" ->> 2) :: ("b" ->> "x") :: HNil)
  }

  test("HList Remove and SplitLeft survive serialization") {
    val l = 1 :: "a" :: true :: HNil
    assertEquals(roundTrip(formless.hlist.Remove[Int :: String :: Boolean :: HNil, String]).apply(l), ("a", 1 :: true :: HNil))
    assertEquals(roundTrip(formless.hlist.SplitLeft[Int :: String :: Boolean :: HNil, String]).apply(l), (1 :: HNil, "a" :: true :: HNil))
  }
}
