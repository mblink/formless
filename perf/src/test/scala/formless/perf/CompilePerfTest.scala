package formless.perf

import dotty.tools.dotc.Driver
import java.lang.management.ManagementFactory
import java.nio.file.Files
import munit.FunSuite

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
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val threads = ManagementFactory.getThreadMXBean match {
    case t: com.sun.management.ThreadMXBean => t
    case other => throw new IllegalStateException(s"The JVM doesn't report thread allocation: ${other.getClass}")
  }

  /** Allocated bytes and CPU nanoseconds to compile `source` on this thread, with JIT warm-up runs first */
  private def compileCost(source: String): (Long, Long) = {
    val dir = Files.createTempDirectory("formless-perf")
    val file = dir.resolve("Bench.scala")
    Files.writeString(file, source)
    def once(): (Long, Long) = {
      val out = Files.createTempDirectory(dir, "out")
      val (a0, c0) = (threads.getCurrentThreadAllocatedBytes, threads.getCurrentThreadCpuTime)
      val reporter = new Driver().process(Array("-usejavacp", "-d", out.toString, file.toString))
      val (a1, c1) = (threads.getCurrentThreadAllocatedBytes, threads.getCurrentThreadCpuTime)
      assert(!reporter.hasErrors, s"the workload didn't compile:\n$source")
      (a1 - a0, c1 - c0)
    }
    (1 to 2).foreach(_ => once())
    val runs = (1 to 3).map(_ => once()).sortBy(_._1)
    runs(1)
  }

  private lazy val emptyCost = compileCost("package formless.bench\nobject Bench\n")

  /** Allocation in MB attributable to the workload, logging it with its CPU time */
  private def workloadMB(name: String, source: String): Double = {
    val (alloc, cpu) = compileCost(source)
    val mb = (alloc - emptyCost._1).toDouble / (1 << 20)
    println(f"$name%-40s ${mb}%8.0f MB allocated, ${(cpu - emptyCost._2) / 1e9}%6.2f s CPU")
    mb
  }

  private def header(imports: String): String = s"package formless.bench\n$imports\nobject Bench {\n"

  private def record(n: Int): String = (0 until n).map(i => s"""("f$i" ->> Int)""").mkString(" :: ") + " :: HNil"

  private def recordLookups(n: Int): String =
    header("import formless.hlist.{::, HNil}\nimport formless.record.*") + s"  type R = ${record(n)}\n" +
      (0 until n).map(i => s"""  val s$i = Selector[R, "f$i"]\n  val u$i = Updater[R, "f$i" ->> Int]\n  val m$i = Modifier[R, "f$i", Int, Long]\n  val r$i = Remover[R, "f$i"]\n""").mkString + "}\n"

  private def recordAppends(n: Int): String =
    header("import formless.hlist.{::, HNil}\nimport formless.record.*") + s"  type R = ${record(n)}\n" +
      (0 until n).map(i => s"""  val a$i = Updater[R, "new$i" ->> Int]\n  val l$i = summon[LacksKey[R, "new$i"]]\n""").mkString + "}\n"

  private def hlistLookups(n: Int): String =
    header("import formless.hlist.{::, HNil, Remove, Replacer, SplitLeft, SplitRight}") +
      s"  type L = ${(0 until n).mkString(" :: ")} :: HNil\n" +
      (0 until n).map(i => s"  val r$i = Remove[L, $i]\n  val p$i = Replacer[L, $i, String]\n  val l$i = SplitLeft[L, $i]\n  val s$i = SplitRight[L, $i]\n").mkString + "}\n"

  private def withinBudget(name: String, source: String, budgetMB: Double): Unit = {
    val mb = workloadMB(name, source)
    assert(mb <= budgetMB, f"$name allocated $mb%.0f MB, over its budget of $budgetMB%.0f MB")
  }

  test("record lookups on an 80-field record stay within budget") {
    withinBudget("320 record lookups, 80 fields", recordLookups(80), budgetMB = 1150)
  }

  test("appends and absence proofs on an 80-field record stay within budget") {
    withinBudget("160 appends and absence proofs, 80 fields", recordAppends(80), budgetMB = 700)
  }

  test("HList element lookups on a 60-element list stay within budget") {
    withinBudget("240 HList element lookups, 60 elements", hlistLookups(60), budgetMB = 550)
  }

  test("the cost of HList element lookups grows about linearly with the list's length") {
    val small = workloadMB("120 HList element lookups, 30 elements", hlistLookups(30))
    val large = workloadMB("240 HList element lookups, 60 elements", hlistLookups(60))
    // Twice as many lookups on a list twice as long cost at most 4 times as much in total if each lookup is linear in the
    // list's length, and 8 if quadratic, as in 0.8. 0.9 measures about 2.7.
    val ratio = large / small
    assert(ratio <= 5.0, f"doubling the list made its lookups cost $ratio%.1f times as much; linear growth is at most 4")
  }
}
