package formless
package hlist

import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

class HListHelperTests extends FunSuite {
  private val values = 1 :: "a" :: true :: HNil

  test("HListOps.apply accepts an explicit index type") {
    assertEquals(HList.toHListOps(values).apply[1], "a")
  }

  test("At evidence requires an in-bounds index") {
    rejects("""At[Int :: HNil, 1]""", "No given instance of type formless.hlist.At")
  }

  test("selectMany accepts a list of singleton indices") {
    val indices = constValueHList[2 :: 0 :: HNil]
    assertEquals(values.selectMany(indices), true :: 1 :: HNil)
  }

  test("HListOps.runtimeLength counts the cells of an HList") {
    assertEquals(HList.toHListOps(values).runtimeLength, 3)
  }

  test("HListOps.toArray preserves element order") {
    val array: Array[Any] = HList.toHListOps(values).toArray[Any]
    assertEquals(array.toList, List[Any](1, "a", true))
  }

  test("HList.length returns the singleton length of a nonempty HList") {
    val length: 3 = HList.length(values)
    assertEquals(length, 3)
  }

  test("HList.length returns singleton zero for HNil") {
    val length: 0 = HList.length(HNil)
    assertEquals(length, 0)
  }

  test("HList.size returns the singleton size of a nonempty HList") {
    val size: 3 = HList.size(values)
    assertEquals(size, 3)
  }

  test("HList.size returns singleton zero for HNil") {
    val size: 0 = HList.size(HNil)
    assertEquals(size, 0)
  }

  test("HList.reverse reverses a heterogeneous list and its type") {
    val reversed: Boolean :: String :: Int :: HNil = HList.reverse(values)
    assertEquals(reversed, true :: "a" :: 1 :: HNil)
  }

  test("HList.fill makes the requested number of copies") {
    val filled: String :: String :: HNil = HList.fill(2)("a")
    assertEquals(filled, "a" :: "a" :: HNil)
  }

  test("HList.fill returns HNil for zero copies") {
    assertEquals(HList.fill(0)("a"), HNil)
  }

  test("HList.fill requires a nonnegative count") {
    rejects("""HList.fill(-1)("a")""", "Cannot prove that")
  }

  test("repeat requires a positive count") {
    rejects("""values.repeat[0]""", "No given instance of type formless.hlist.Repeat")
  }

  test("take rejects a count greater than the length") {
    rejects("""values.take[4]""", "No given instance of type formless.hlist.Take")
  }

  test("drop rejects a count greater than the length") {
    rejects("""values.drop[4]""", "No given instance of type formless.hlist.Drop")
  }

  test("split rejects an index past the end") {
    rejects("""values.split[4]""", "No given instance of type formless.hlist.Split")
  }

  test("reverse_split rejects an index past the end") {
    rejects("""values.reverse_split[4]""", "No given instance of type formless.hlist.ReverseSplit")
  }

  test("selectMany rejects an index outside the HList") {
    rejects("""values.selectMany(constValueHList[2 :: 3 :: HNil])""", "No given instance of type formless.hlist.SelectMany")
  }

  test("Comapped requires every element to have the specified type constructor") {
    rejects("""Comapped[Option[Int] :: String :: HNil, Option]""", "No given instance of type formless.hlist.Comapped")
  }

  test("IsHCons requires a nonempty HList") {
    rejects("""IsHCons[HNil]""", "No given instance of type formless.hlist.IsHCons")
  }

  private inline def rejects(inline code: String, message: String)(using munit.Location): Unit = {
    val errors = typeCheckErrors(code).map(_.message)
    assertEquals(errors.map(_.take(message.length)), List(message), errors.mkString("\n"))
  }
}
