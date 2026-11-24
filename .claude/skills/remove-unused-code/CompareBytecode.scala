//> using dep org.ow2.asm:asm:9.11.1
//> using dep org.ow2.asm:asm-util:9.11.1
import java.io.{PrintWriter, StringWriter}
import java.nio.file.{Files, Path, Paths}
import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class ClassText(source: String, header: String, members: Map[String, String])

def read(p: Path): ClassText = {
  val members = mutable.Map.empty[String, String]
  var source = ""
  var header = ""
  new ClassReader(Files.readAllBytes(p)).accept(new ClassVisitor(Opcodes.ASM9) {
    override def visit(v: Int, a: Int, name: String, sig: String, sup: String, is: Array[String]): Unit =
      header = s"$a $name $sig $sup ${Option(is).fold("")(_.mkString(","))}"
    override def visitSource(src: String, debug: String): Unit = source = Option(src).getOrElse("")
    override def visitField(a: Int, n: String, d: String, s: String, v: Object) = { members(s"field $n $d") = s"$a $s $v"; null }
    override def visitMethod(a: Int, n: String, d: String, s: String, e: Array[String]): MethodVisitor = {
      val t = new Textifier()
      new MethodVisitor(Opcodes.ASM9, new TraceMethodVisitor(t)) {
        override def visitEnd(): Unit = {
          super.visitEnd()
          val sw = new StringWriter(); t.print(new PrintWriter(sw)); members(s"method $n$d") = s"$a\n$sw"
        }
      }
    }
  }, ClassReader.SKIP_FRAMES)
  ClassText(source, header, members.toMap)
}

// Drops debug-only lines (line numbers, local variable and parameter names), and makes the numbers in compiler-
// generated names (`$anonfun$10`, `a$proxy9$1`, `_$19`) equal: they depend on what else was compiled in the same run.
// Default getters keep theirs (`bar$default$2`), since which default is read matters. This can't tell apart a lambda
// that now calls a different one of two generated helpers, or two local `given`s of the same type (`given_X$1`).
def strip(s: String): String =
  synthetic(
    s.linesIterator
      .filterNot(l => l.trim.startsWith("LINENUMBER") || l.trim.startsWith("LOCALVARIABLE") || l.trim.startsWith("// parameter") || l.trim.matches("L\\d+"))
      .map(_.replaceAll("\\bL\\d+\\b", "L"))
      .mkString("\n")
  )

def synthetic(s: String): String =
  s.replaceAll("(?<!\\$default)\\$\\d+", "\\$N").replaceAll("(proxy|lzy|lzyINIT)\\d+", "$1N")

def classes(root: Path): Map[String, Path] =
  Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".class")).map(p => root.relativize(p).toString -> p).toMap

// Usage: scala-cli run CompareBytecode.scala -- <before-root> <after-root> <file listing class dirs relative to the roots>
//
// Classes are matched by name with compiler-generated numbers made equal (so `Foo$$anon$3` and `Foo$$anon$4` are
// compared together), and each group's members are compared as a multiset.
@main def diff(beforeRoot: String, afterRoot: String, dirsFile: String): Unit = {
  val dirs = scala.io.Source.fromFile(dirsFile).getLines().map(_.stripPrefix("./")).filter(_.nonEmpty).toList
  // No directories would look like no change
  if (dirs.isEmpty) sys.error(s"No class directories listed in $dirsFile")
  // A directory missing from both roots means the list or a root is wrong, which would otherwise look like no change
  val missing = dirs.filterNot(d => Files.isDirectory(Paths.get(beforeRoot, d)) || Files.isDirectory(Paths.get(afterRoot, d)))
  if (missing.nonEmpty) sys.error(s"Missing from both roots: ${missing.mkString(", ")}")
  dirs.filterNot(d => Files.isDirectory(Paths.get(beforeRoot, d)) && Files.isDirectory(Paths.get(afterRoot, d)))
    .foreach(d => println(s"MISSING from one root, compared as empty: $d"))
  def grouped(root: Path, dir: String): Map[String, List[ClassText]] = {
    val d = root.resolve(dir)
    if (Files.isDirectory(d)) classes(d).toList.groupMap((k, _) => synthetic(k))((_, p) => read(p)) else Map.empty
  }
  def members(ts: List[ClassText]): Map[String, Int] =
    ts.flatMap(t => t.header.split(" ").headOption.map(a => s"class $a ${synthetic(t.header)}").toList ++
      t.members.toList.map((m, body) => s"${synthetic(m)}\n${strip(body)}"))
      .groupMapReduce(identity)(_ => 1)(_ + _)
  for (dir <- dirs) {
    val (b, a) = (grouped(Paths.get(beforeRoot), dir), grouped(Paths.get(afterRoot), dir))
    for (k <- (b.keySet ++ a.keySet).toList.sorted) {
      val pkg = k.split('/').dropRight(1).mkString("/")
      val source = (a.getOrElse(k, Nil) ++ b.getOrElse(k, Nil)).headOption.fold("")(_.source)
      val (bm, am) = (members(b.getOrElse(k, Nil)), members(a.getOrElse(k, Nil)))
      for (m <- (bm.keySet ++ am.keySet).toList.sorted) {
        val (bc, ac) = (bm.getOrElse(m, 0), am.getOrElse(m, 0))
        val name = m.takeWhile(_ != '\n')
        if (bc > ac) println(s"REMOVED\t$dir\t$k\t$source\t$pkg\t$name")
        if (ac > bc) println(s"ADDED\t$dir\t$k\t$source\t$pkg\t$name")
      }
    }
  }
}
