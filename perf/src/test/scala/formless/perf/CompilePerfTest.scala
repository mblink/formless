package formless.perf

import dotty.tools.dotc.Driver
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.FutureTask
import munit.FunSuite
import scala.util.Using

/**
 * Compile-time regression tests for the record and HList lookups. Each workload is a generated source file that the
 * Scala 3 compiler compiles in this JVM; the test measures the memory the compile allocates, which stays within about
 * 1% from run to run, unlike CPU or wall time on a shared CI machine. A workload's cost is its allocation minus that of
 * an empty file, so the compiler's own start-up doesn't count.
 *
 * The budgets are about twice what 0.9 allocates, and well below 0.8, which allocated 3 to 35 times as much; the
 * scaling test catches a lookup whose cost grows faster than linearly with the list's length, as 0.8's did.
 */
class CompilePerfTest extends FunSuite {
  import CompilePerfTest.*

  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  // Checked once here, so a JVM that can't count a thread's allocations fails the suite rather than reporting 0 bytes
  private lazy val threads: com.sun.management.ThreadMXBean = ManagementFactory.getThreadMXBean match {
    case t: com.sun.management.ThreadMXBean if t.isThreadAllocatedMemorySupported && t.isThreadCpuTimeSupported =>
      t.setThreadAllocatedMemoryEnabled(true)
      t.setThreadCpuTimeEnabled(true)
      t
    case other => fail(s"The JVM doesn't report a thread's allocation and CPU time: ${other.getClass}")
  }

  private given Using.Releasable[Path] = dir =>
    Using.resource(Files.walk(dir))(_.sorted(Comparator.reverseOrder[Path]).forEach(p => Files.delete(p)))

  /** The median of `Samples` measurements to compile `source` on this thread, after `WarmUps` JIT warm-up runs */
  private def compileCost(source: String): Cost =
    Using.resource(Files.createTempDirectory("formless-perf")) { dir =>
      val file = dir.resolve("Bench.scala")
      Files.writeString(file, source)
      def once(): Cost = {
        val out = Files.createTempDirectory(dir, "out")
        val before = Cost(threads.getCurrentThreadAllocatedBytes, threads.getCurrentThreadCpuTime)
        val reporter = new Driver().process(Array("-usejavacp", "-d", out.toString, file.toString))
        val after = Cost(threads.getCurrentThreadAllocatedBytes, threads.getCurrentThreadCpuTime)
        assert(!reporter.hasErrors, s"the workload didn't compile:\n$source")
        after - before
      }
      (1 to WarmUps).foreach(_ => once())
      (1 to Samples).map(_ => once()).sortBy(_.bytes).apply(Samples / 2)
    }

  private lazy val emptyCost = compileCost("package formless.bench\nobject Bench\n")

  /** Allocation in MB attributable to the workload, logging it with its CPU time */
  private def workloadMB(w: Workload): Double = {
    val cost = compileCost(w.source) - emptyCost
    val mb = cost.bytes.toDouble / (1 << 20)
    println(f"${w.name}%-45s ${mb}%8.0f MB allocated, ${cost.cpuNanos / 1e9}%6.2f s CPU")
    mb
  }

  private def withinBudget(w: Workload, budgetMB: Double): Unit = {
    val mb = workloadMB(w)
    assert(mb <= budgetMB, f"${w.name} allocated $mb%.0f MB, over its budget of $budgetMB%.0f MB")
  }

  test("record lookups on an 80-field record stay within budget") {
    withinBudget(Workload(Kind.RecordLookups, 80), budgetMB = 1150)
  }

  test("appends and absence proofs on an 80-field record stay within budget") {
    withinBudget(Workload(Kind.RecordAppends, 80), budgetMB = 700)
  }

  test("HList element lookups on a 60-element list stay within budget") {
    withinBudget(Workload(Kind.HListLookups, 60), budgetMB = 550)
  }

  test("the memory HList element lookups allocate grows about linearly with the list's length") {
    val small = Workload(Kind.HListLookups, 30)
    val ratio = workloadMB(small.doubled) / workloadMB(small)
    // Twice as many lookups on a list twice as long allocate at most 4 times as much in total if each lookup is linear
    // in the list's length, and 8 if quadratic, as in 0.8. 0.9 measures about 2.7.
    assert(ratio <= 5.0, f"doubling the list made its lookups allocate $ratio%.1f times as much; linear growth is at most 4")
  }

  test("long record and union lookups compile with a one MiB stack") {
    Using.resource(Files.createTempDirectory("formless-stack")) { dir =>
      val file = dir.resolve("LongRecord.scala")
      Files.writeString(file, longRecordSource)
      val compile = new FutureTask[Boolean](() =>
        !new Driver().process(Array("-usejavacp", "-d", dir.toString, file.toString)).hasErrors,
      )
      // Leave the larger stack used by the allocation measurements out of this compiler run.
      val thread = new Thread(Thread.currentThread().getThreadGroup, compile, "formless-small-stack-compiler", 1024L * 1024)
      thread.start()
      assert(compile.get(), "long record lookups didn't compile")
    }
  }
}

object CompilePerfTest {
  private val WarmUps = 2
  private val Samples = 3

  private def longRecordSource: String = {
    val size = 250
    // Short aliases isolate lookup from the compiler's recursion through a single deep infix type tree.
    val aliases = (0 until size).grouped(25).map { fields =>
      val next = fields.last + 1
      val tail = if (next == size) """("target" ->> Int) :: HNil""" else s"Fields$next[V]"
      val heads = fields.map { i =>
        val value = if (i == size - 1) "V" else "Int"
        s"""("f$i" ->> $value)"""
      }
      s"  type Fields${fields.head}[V] = ${(heads :+ tail).mkString(" :: ")}"
    }.mkString("\n")
    s"""package formless.bench
       |import formless.hlist.{::, HNil, Remove}
       |import formless.record.*
       |object LongRecord {
       |$aliases
       |  type A = ("a" ->> Int) :: Fields0[Int]
       |  type B = ("b" ->> Int) :: Fields0[String]
       |  type R = A | B
       |  val a = Selector[A, "target"]
       |  val b = Selector[R, "f${size - 1}"]
       |  summon[b.Out =:= (Int | String)]
       |  val c = Updater[R, "new" ->> Int]
       |  val d = Remove[R, "target" ->> Int]
       |}
       |""".stripMargin
  }

  final case class Cost(bytes: Long, cpuNanos: Long) {
    def -(other: Cost): Cost = Cost(bytes - other.bytes, cpuNanos - other.cpuNanos)
  }

  /** A kind of generated workload, with how many lookups it makes per element */
  enum Kind(val lookupsPerElement: Int, val lookups: String, val elements: String) {
    case RecordLookups extends Kind(4, "record lookups", "fields")
    case RecordAppends extends Kind(2, "appends and absence proofs", "fields")
    case HListLookups extends Kind(4, "HList element lookups", "elements")
  }

  /** A generated source file making `kind`'s lookups on a record or list of `size` elements */
  final case class Workload(kind: Kind, size: Int) {
    def name: String = s"${kind.lookupsPerElement * size} ${kind.lookups}, $size ${kind.elements}"
    def doubled: Workload = copy(size = size * 2)

    def source: String = kind match {
      case Kind.RecordLookups =>
        header("import formless.hlist.{::, HNil}\nimport formless.record.*") + s"  type R = $record\n" +
          (0 until size).map(i => s"""  val s$i = Selector[R, "f$i"]\n  val u$i = Updater[R, "f$i" ->> Int]\n  val m$i = Modifier[R, "f$i", Int, Long]\n  val r$i = Remover[R, "f$i"]\n""").mkString + "}\n"
      case Kind.RecordAppends =>
        header("import formless.hlist.{::, HNil}\nimport formless.record.*") + s"  type R = $record\n" +
          (0 until size).map(i => s"""  val a$i = Updater[R, "new$i" ->> Int]\n  val l$i = summon[LacksKey[R, "new$i"]]\n""").mkString + "}\n"
      case Kind.HListLookups =>
        header("import formless.hlist.{::, HNil, Remove, Replacer, SplitLeft, SplitRight}") +
          s"  type L = ${(0 until size).mkString(" :: ")} :: HNil\n" +
          (0 until size).map(i => s"  val r$i = Remove[L, $i]\n  val p$i = Replacer[L, $i, String]\n  val l$i = SplitLeft[L, $i]\n  val s$i = SplitRight[L, $i]\n").mkString + "}\n"
    }

    private def header(imports: String): String = s"package formless.bench\n$imports\nobject Bench {\n"
    private def record: String = (0 until size).map(i => s"""("f$i" ->> Int)""").mkString(" :: ") + " :: HNil"
  }
}
