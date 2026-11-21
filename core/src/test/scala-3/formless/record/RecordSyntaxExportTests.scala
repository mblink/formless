package formless.record.syntaxtest

import formless.record.syntax.*
import munit.FunSuite

class RecordSyntaxExportTests extends FunSuite {
  test("the syntax import exports the labelled field conversion") {
    val convert = ->>
    val field: "name" ->> String = "name" ->> "Ada"
    assertEquals(convert.inlineConvertToV(field), "Ada")
  }
}
