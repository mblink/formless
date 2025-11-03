package formless
package test

import munit.Assertions.assert
import munit.Location
import scala.compiletime.testing.typeChecks

/**
 * A utility which ensures that a code fragment does not typecheck.
 */
object illTyped {
  inline def apply(code: String)(using loc: Location): Unit = assert(!typeChecks(code))
}
