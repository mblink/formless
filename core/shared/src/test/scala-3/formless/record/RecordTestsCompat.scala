package formless.record

trait RecordTestsCompat { self: RecordTests =>
  test("FieldTypeOfValueClass") {
    val x = RecordTests.aValueClassField ->> RecordTests.AValueClass(1L)
    assertEquals(x.l, Array(x).apply(0).l)
  }
}
