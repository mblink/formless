package formless
package test

import scala.language.experimental.macros

/**
 * A utility which ensures that a code fragment does not typecheck.
 */
object illTyped {
  def apply(code: String): Unit = macro shapeless.test.IllTypedMacros.applyImplNoExp
  def apply(code: String, expected: String): Unit = macro shapeless.test.IllTypedMacros.applyImpl
}
