package formless.hlist

import munit.FunSuite

class LongHListTests extends FunSuite {
  private val size = 100000
  private val middle = size / 2
  private val values = HList.fromArray(Array.tabulate(size)(identity))
  private val list = -1 :: values

  test("length, array conversion, rendering and reversal handle a long spine") {
    assertEquals(list.runtimeLength, 100001)
    assertEquals(HList.length(list): Int, 100001)
    assertEquals(list.toArray.last, 99999)
    assert(list.toString.endsWith("99999 :: HNil"))
    assertEquals(HList.reverse(list).unsafeApply(0), 99999)
    assertEquals(HList.last(list): Any, 99999)
  }

  test("nested HLists render without recursive toString calls") {
    val nested = (0 until size).foldLeft(HNil: HList)((inner, _) => inner :: HNil)
    val rendered = nested.toString
    assert(rendered.startsWith("((("))
    assert(rendered.endsWith(") :: HNil) :: HNil"))
  }

  test("prefix copies handle a long spine and keep the original suffix") {
    val (prefix, suffix) = HList.splitBefore(values, middle)
    assertEquals(prefix.runtimeLength, 50000)
    assertEquals(suffix.unsafeApply(0), 50000)
    assertEquals(HList.concat(prefix, suffix).unsafeApply(middle), 50000)
    assertEquals(HList.init(list).unsafeApply(99999), 99998)
    assertEquals(HList.append(values, "last").unsafeApply(size), "last")

    val (throughMatch, afterMatch) = HList.splitAfter(values, middle)
    assertEquals(throughMatch.unsafeApply(middle), 50000)
    assertEquals(afterMatch.unsafeApply(0), 50001)
    val (reversed, sameSuffix) = HList.reverseSplitBefore(values, middle)
    assertEquals(reversed.unsafeApply(0), 49999)
    assert(sameSuffix eq suffix)
    val (reverseThroughMatch, sameAfterMatch) = HList.reverseSplitAfter(values, middle)
    assertEquals(reverseThroughMatch.unsafeApply(0), 50000)
    assert(sameAfterMatch eq afterMatch)
  }

  test("indexed edits handle a long spine") {
    val (removed, rest) = HList.removedAt(values, middle)
    assertEquals(removed, 50000)
    assertEquals(rest.unsafeApply(middle), 50001)
    val reinserted = HList.insertedAt(rest, middle, removed)
    assertEquals(reinserted.runtimeLength, 100000)
    assertEquals(reinserted.unsafeApply(middle), 50000)
    assertEquals(HList.updatedAt[Int](values, middle, _ + 1).unsafeApply(middle), 50001)
    val (original, replaced) = HList.replacedAt[Int](values, middle, _.toString)
    assertEquals(original, 50000)
    assertEquals(replaced.unsafeApply(middle), "50000")
  }
}
