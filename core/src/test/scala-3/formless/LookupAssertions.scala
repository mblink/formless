package formless

import munit.{Assertions, Location}
import scala.compiletime.testing.typeCheckErrors

/** Assertions about how a lookup's code compiles, shared by the lookup test suites */
trait LookupAssertions { self: Assertions =>
  // The messages of the lookups' `@implicitNotFound`s and `Lookup.LookupError`, so an unrelated error such as an unknown
  // name doesn't count as a refusal
  def isLookupError(message: String): Boolean =
    message.startsWith("Cannot ") || message.startsWith("Failed to find") || message.startsWith("Record ")

  /** `code` fails to compile with a lookup's error; a crash during macro expansion fails too, so only a clean refusal passes */
  inline def refused(inline code: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.exists(isLookupError), s"expected a lookup's compile error: $code\n${errs.mkString("\n")}")
    assert(!errs.exists(_.contains("Exception occurred while executing macro expansion")), errs.mkString("\n"))
  }

  /** `code` fails to compile with an error containing `expected` */
  inline def abortsWith(inline code: String, expected: String)(using loc: Location): Unit = {
    val errs = typeCheckErrors(code).map(_.message)
    assert(errs.exists(_.contains(expected)), errs.mkString("\n"))
  }

  inline def compiles(inline code: String)(using loc: Location): Unit =
    assertEquals(typeCheckErrors(code).map(_.message), Nil)
}
