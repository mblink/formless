package formless.record

import formless.hlist.HNil

trait RecordTestsCompat { self: RecordTests =>
  test("merging two empty records returns HNil") {
    assertEquals(HNil.merge(HNil), HNil)
  }

  test("merging HLists appends unlabelled elements even when their types repeat") {
    val left = ("name" ->> "Dune") :: 1 :: HNil
    val right = 2 :: ("name" ->> "Arrakis") :: HNil
    assertEquals(left.merge(right), ("name" ->> "Arrakis") :: 1 :: 2 :: HNil)
  }
}
