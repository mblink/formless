// Removes the default values find-unused reports as unused, e.g. `com.example.Foo.bar(baz = ...)`.
//
// Usage: scala-cli run RemoveDefaults.scala -- findings.tsv [--apply]
//
// `findings.tsv` is `table2tsv.sh` output from a run without `--transitive` (in transitive mode, a default that only
// unused code relies on is reported too, and removing it breaks that code). Rows that aren't default values are
// ignored. Each location is the parameter's name; the default is everything after the first `=` at bracket depth 0, up
// to the next `,` or closing bracket, not counting a trailing comment. Without `--apply` it only prints what it would
// remove; review that before applying. Every edit is computed before any file is written, and a default it can't parse
// is reported and left alone.
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class ParseError(msg: String) extends Exception(msg)

val charLiteral = raw"'(?:\\u[0-9a-fA-F]{4}|\\.|[^\\'\n])'".r

// The index after the `}` that closes the `${` of an interpolated string, whose `{` is at `i`
def skipSplice(s: String, i: Int): Int = {
  var depth = 0
  var j = i
  while (j < s.length) {
    val k = skipLiteralOrComment(s, j)
    if (k != j) j = k
    else {
      if (s(j) == '{') depth += 1
      else if (s(j) == '}') {
        depth -= 1
        if (depth == 0) return j + 1
      }
      j += 1
    }
  }
  throw new ParseError("unterminated string")
}

// The index after the string, char literal or comment starting at `i`, or `i` if there's none there
def skipLiteralOrComment(s: String, i: Int): Int = {
  // `s"..."`, `f"..."` and the like can contain `${...}`, which can contain strings of its own
  val interpolated = i > 0 && (s(i - 1).isLetterOrDigit || s(i - 1) == '_')
  if (s.startsWith("\"\"\"", i)) {
    var j = i + 3
    while (!s.startsWith("\"\"\"", j)) {
      if (j >= s.length) throw new ParseError("unterminated string")
      j = if (interpolated && s.startsWith("${", j)) skipSplice(s, j + 1) else j + 1
    }
    // A triple-quoted string can end with more quotes, which belong to it
    while (s.startsWith("\"\"\"\"", j)) j += 1
    j + 3
  } else if (s(i) == '"') {
    var j = i + 1
    while (j < s.length && s(j) != '"') {
      if (s(j) == '\n') throw new ParseError("unterminated string")
      j = if (interpolated && s.startsWith("${", j)) skipSplice(s, j + 1) else j + (if (s(j) == '\\') 2 else 1)
    }
    if (j >= s.length) throw new ParseError("unterminated string")
    j + 1
  } else if (s(i) == '\'') {
    charLiteral.findPrefixMatchOf(s.substring(i)).fold(i)(m => i + m.end)
  } else if (s.startsWith("//", i)) {
    val end = s.indexOf('\n', i)
    if (end < 0) s.length else end
  } else if (s.startsWith("/*", i)) {
    var depth = 1
    var j = i + 2
    while (depth > 0) {
      if (j >= s.length) throw new ParseError("unterminated comment")
      if (s.startsWith("/*", j)) { depth += 1; j += 2 }
      else if (s.startsWith("*/", j)) { depth -= 1; j += 2 }
      else j += 1
    }
    j
  } else i
}

// The `=` that starts the default value of the parameter whose type starts at `start`
def findDefaultEquals(s: String, start: Int): Int = {
  var depth = 0
  var i = start
  while (i < s.length) {
    val j = skipLiteralOrComment(s, i)
    if (j != i) i = j
    else {
      val ch = s(i)
      if ("([{".contains(ch)) depth += 1
      else if (")]}".contains(ch)) {
        if (depth == 0) throw new ParseError("no default value found")
        depth -= 1
      } else if (ch == ',' && depth == 0) throw new ParseError("no default value found")
      else if (ch == '=' && depth == 0 && !"=!<>:".contains(s(i - 1)) && (i + 1 >= s.length || !"=>:".contains(s(i + 1))))
        return i
      i += 1
    }
  }
  throw new ParseError("no default value found")
}

// The end of the last code (not whitespace or a comment) of the default value starting at `start`
def defaultEnd(s: String, start: Int): Int = {
  var depth = 0
  var i = start
  var lastCode = start
  while (i < s.length) {
    val j = skipLiteralOrComment(s, i)
    if (j != i) {
      if (!(s.startsWith("//", i) || s.startsWith("/*", i))) lastCode = j
      i = j
    } else {
      val ch = s(i)
      if ("([{".contains(ch)) depth += 1
      else if (")]}".contains(ch)) {
        if (depth == 0) return lastCode
        depth -= 1
      } else if (ch == ',' && depth == 0) return lastCode
      if (!ch.isWhitespace) lastCode = i + 1
      i += 1
    }
  }
  throw new ParseError("no end of the default value found")
}

// The span to delete for the default of `param`, whose name starts at `start`
def defaultRemoval(s: String, start: Int, param: String): (Int, Int) = {
  val name = if (s.startsWith("`", start)) s"`$param`" else param
  if (!s.startsWith(name, start))
    throw new ParseError(s"expected `$param` at the reported position, found ${s.substring(start, math.min(s.length, start + 30))}")
  val eq = findDefaultEquals(s, start + name.length)
  var b = eq
  while (s(b - 1) == ' ' || s(b - 1) == '\t') b -= 1
  (b, defaultEnd(s, eq + 1))
}

@main def removeDefaults(args: String*): Unit = {
  val apply = args.contains("--apply")
  val file = args.find(_ != "--apply").getOrElse(sys.error("Usage: RemoveDefaults.scala findings.tsv [--apply]"))
  val cwd = Paths.get("").toAbsolutePath
  def rel(p: String): String = cwd.relativize(Paths.get(p).toAbsolutePath).toString

  val byFile = Files.readAllLines(Paths.get(file)).asScala.toList
    .flatMap { line =>
      val (name, tab, loc) = line.span(_ != '\t') match { case (n, rest) => (n, rest.take(1), rest.drop(1)) }
      val parts = loc.split(":")
      if (tab.isEmpty || !name.endsWith(" = ...)") || parts.length < 3) None
      else Some((parts.dropRight(2).mkString(":"), parts(parts.length - 2).toInt, parts.last.toInt, name))
    }
    .groupBy(_._1)

  var failures = 0
  val edits = byFile.toList.map { case (path, items) =>
    var s = Files.readString(Paths.get(path))
    val offsets = s.split("\n", -1).scanLeft(0)((o, ln) => o + ln.length + 1)
    val spans = items.map { case (_, l, c, name) => (l, c, name) }.sorted.flatMap { case (l, c, name) =>
      val param = name.split("\\(").last.split(" = ").head
      try {
        val (b, e) = defaultRemoval(s, offsets(l - 1) + c - 1, param)
        println(s"${rel(path)}:$l: $name: removing '${s.substring(b, e)}'")
        Some((b, e))
      } catch {
        case err: ParseError =>
          println(s"${rel(path)}:$l: $name: SKIPPED, ${err.getMessage}")
          failures += 1
          None
      }
    }
    // Last first, so earlier offsets stay valid
    spans.sortBy(-_._1).foreach((b, e) => s = s.substring(0, b) + s.substring(e))
    path -> s
  }

  if (apply) edits.foreach((path, s) => Files.writeString(Paths.get(path), s))
  if (failures > 0) println(s"$failures default(s) skipped")
}
