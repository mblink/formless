package formless

trait SourceGenerator {
  def apply(): String
}

object SourceGenerator {
  val tpes = List("Int", "String", "Boolean")
  val hlistLimit = 12

  case object Util extends SourceGenerator {
    def apply(): String = s"""
package formless.test

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.propBoolean
import formless.hlist._
import formless.record._

object Util {
  def compare(expected: Any, actual: Any): Prop = (actual == expected) :| s"Expected: $$expected, actual: $$actual"

  implicit def labelledArb[K, V](implicit V: Arbitrary[V]): Arbitrary[K ->> V] = V.asInstanceOf[Arbitrary[K ->> V]]

  private implicit val hnilArb: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))

  private implicit def hconsArb[H: Arbitrary, T <: HList: Arbitrary]: Arbitrary[H :: T] =
    Arbitrary(for {
      h <- Arbitrary.arbitrary[H]
      t <- Arbitrary.arbitrary[T]
    } yield h :: t)

  ${1.to(hlistLimit).map { i =>
    val hlistTpe = 1.to(i).foldRight("HNil")(
      (j, acc) => s"""("k$j" ->> ${tpes(j % tpes.length)}) :: $acc""")
    s"""
  type X$i = $hlistTpe
  implicit val x${i}Arb: Arbitrary[X$i] = hconsArb
"""
  }.mkString("\n")}
}
"""
  }

  case object HListSelectorTest extends SourceGenerator {
    def apply(): String = s"""
package formless.hlist

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.record.->>
import formless.test.Util._

object SelectorTest extends Properties("hlist.Selector") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = s""""k$j" ->> ${tpes(j % tpes.length)}"""
      s"""

  property.update("select ${j - 1} from HList$i", Prop.forAll { (x: X$i) =>
    compare(x.toList.apply(${j - 1}), Selector[X$i, $tpe].apply(x))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object RecordSelectorTest extends SourceGenerator {
    def apply(): String = s"""
package formless.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.test.Util._

object SelectorTest extends Properties("record.Selector") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      s"""

  property.update("select k$j from HList$i", Prop.forAll { (x: X$i) =>
    compare(x.toList.apply(${j - 1}), Selector[X$i, "k$j"].apply(x))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object UpdaterTest extends SourceGenerator {
    def apply(): String = s"""
package formless.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.test.Util._

object UpdaterTest extends Properties("Updater") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      s"""

  property.update("update k$j in HList$i", Prop.forAll { (x: X$i, a: $tpe) =>
    val upd = Updater[X$i, "k$j" ->> $tpe].apply(x, label["k$j"](a))
    compare(a, upd.toList.apply(${j - 1}))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object ModifierTest extends SourceGenerator {
    def apply(): String = s"""
package formless.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.test.Util._

object ModifierTest extends Properties("Modifier") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("modify k$j in HList$i", Prop.forAll { (x: X$i, a: $nextTpe) =>
    val mod = Modifier[X$i, "k$j", $tpe, $nextTpe].apply(x, _ => a)
    compare(a, mod.toList.apply(${j - 1}))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object RenamerTest extends SourceGenerator {
    def apply(): String = s"""
package formless.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.test.Util._

object RenamerTest extends Properties("Renamer") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("rename k$j in HList$i", Prop.forAll { (x: X$i, k: String) =>
    val renamed = Renamer[X$i, "k$j", k.type].apply(x)
    compare(renamed.toList.apply(${j - 1}), Selector[renamed.type, k.type].apply(renamed))
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }

  case object RemoverTest extends SourceGenerator {
    def apply(): String = s"""
package formless.record

import org.scalacheck.{Prop, Properties}
import scala.util.chaining._
import formless.hlist.HNil
import formless.test.Util._

object RemoverTest extends Properties("Remover") {
  ${1.to(hlistLimit).flatMap { i =>
    1.to(i).toList.map { j =>
      val tpe = tpes(j % tpes.length)
      val nextTpe = tpes((j + 1) % tpes.length)
      s"""

  property.update("remove k$j from HList$i", Prop.forAll { (x: X$i) =>
    val removed = Remover[X$i, "k$j"].apply(x)._2
    val removedL = removed.toList
    (true: Prop)${if ((1.to(j - 1) ++ (j + 1).to(i)).nonEmpty) " &&" else " && removedL.isEmpty && compare(removed, HNil)"}
      ${(
        1.to(j - 1).map(k => s"""compare(removedL(${k - 1}), Selector[removed.type, "k$k"].apply(removed))""") ++
        (j + 1).to(i).map(k => s"""compare(removedL(${k - 2}), Selector[removed.type, "k$k"].apply(removed))""")
      ).mkString(" &&\n      ")}
  }).pipe(_ => ())
  """
    }
  }.mkString("\n")}
}
"""
  }
}
