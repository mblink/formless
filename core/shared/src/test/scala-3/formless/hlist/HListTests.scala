package formless
package hlist

import munit.{FunSuite, Location}
import formless.test.*
import formless.testutil.*

class HListTests extends FunSuite {
  type SI = Set[Int] :: HNil
  type OI = Option[Int] :: HNil

  type III = Int :: Int :: Int :: HNil

  type SISS = Set[Int] :: Set[String] :: HNil
  type OIOS = Option[Int] :: Option[String] :: HNil

  type ISII = Int :: String :: Int :: Int :: HNil
  type IIII = Int :: Int :: Int :: Int :: HNil
  type IYII = Int :: Any :: Int :: Int :: HNil

  type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
  type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

  type BBBB = Boolean :: Boolean :: Boolean :: Boolean :: HNil

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  type YYYY = Any :: Any :: Any :: Any :: HNil
  type FF = Fruit :: Fruit :: HNil
  type AP = Apple :: Pear :: HNil
  type BP = Banana :: Pear :: HNil
  type AF = Apple :: Fruit :: HNil
  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil
  type APBP = Apple :: Pear :: Banana :: Pear :: HNil
  type APB = Apple :: Pear :: Banana :: HNil
  type PBPA = Pear :: Banana :: Pear :: Apple :: HNil
  type PABP = Pear :: Apple :: Banana :: Pear :: HNil

  val a: Apple = Apple()
  val p: Pear = Pear()
  val b: Banana = Banana()
  val f: Fruit = new Fruit {}

  val ap: AP = a :: p :: HNil
  val bp: BP = b :: p :: HNil
  val apap: APAP = a :: p :: a :: p :: HNil
  val apbp: APBP = a :: p :: b :: p :: HNil
  val apapList = List(a, p, a, p)
  val apbpList = List(a, p, b, p)
  val apapArray = Array(a, p, a, p)
  val apbpArray = Array(a, p, b, p)

  trait Ctv[-T]
  type CICSCICICD = Ctv[Int] :: Ctv[String] :: Ctv[Int] :: Ctv[Int] :: Ctv[Double] :: HNil

  val ci: Ctv[Int] = new Ctv[Int] {}
  val cs: Ctv[String] = new Ctv[String] {}
  val cd: Ctv[Double] = new Ctv[Double] {}
  val cicscicicdList = List(ci, cs, ci, ci, cd)
  val cicscicicdArray = Array(ci, cs, ci, ci, cd)
  val cicscicicd: CICSCICICD = ci :: cs :: ci :: ci :: cd :: HNil

  trait M[T]
  type MIMSMIMIMD = M[Int] :: M[String] :: M[Int] :: M[Int] :: M[Double] :: HNil

  val mi: M[Int] = new M[Int] {}
  val ms: M[String] = new M[String] {}
  val md: M[Double] = new M[Double] {}
  val mimsmimimdList = List(mi, ms, mi, mi, md)
  val mimsmimimdArray = Array(mi, ms, mi, mi, md)
  val mimsmimimd: MIMSMIMIMD = mi :: ms :: mi :: mi :: md :: HNil

  import language.existentials
  val mExist: M[?] = new M[Double] {}
  type MIMSMIMEMD = M[Int] :: M[String] :: M[Int] :: M[?] :: M[Double] :: HNil
  val mimsmimemdList = List(mi, ms, mi, mExist, md)
  val mimsmimemdArray = Array[M[?]](mi, ms, mi, mExist, md)
  val mimsmimemd: MIMSMIMEMD = mi :: ms :: mi :: mExist :: md :: HNil

  trait M2[A,B]
  type M2IM2SM2IM2IM2D = M2[Int, Unit] :: M2[String, Unit] :: M2[Int, Unit] :: M2[Int, Unit] :: M2[Double, Unit] :: HNil

  val m2i: M2[Int, Unit] = new M2[Int, Unit] {}
  val m2s: M2[String, Unit] = new M2[String, Unit] {}
  val m2d: M2[Double, Unit] = new M2[Double, Unit] {}
  val m2im2sm2im2im2dList = List(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2dArray = Array(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2d: M2IM2SM2IM2IM2D = m2i :: m2s :: m2i :: m2i :: m2d :: HNil

  val m2iExist: M2[Int, ?] = new M2[Int, Unit] {}
  val m2sExist: M2[String, ?] = new M2[String, Unit] {}
  val m2dExist: M2[Double, ?] = new M2[Double, Unit] {}
  type M2EIM2ESM2EIM2EEM2ED = M2[Int, ?] :: M2[String, ?] :: M2[Int, ?] :: M2[Int, ?] :: M2[Double, ?] :: HNil
  val m2eim2esm2eim2eem2edList = List(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2edArray = Array(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2ed: M2EIM2ESM2EIM2EEM2ED = m2iExist :: m2sExist :: m2iExist :: m2iExist :: m2dExist :: HNil

  object mkString extends (Any -> String)(_.toString)
  object fruit extends (Fruit -> Fruit)(f => f)
  object incInt extends (Int >-> Int)(_ + 1)

  test("Basics") {
    val l = 1 :: "foo" :: 2.0 :: HNil

    val r1 = l.head
    assertTypedEquals[Int](1, r1)

    val r2 = l.tail.head
    assertTypedEquals[String]("foo", r2)

    assertEqualsDouble(2.0, l.tail.tail.head, Double.MinPositiveValue)

    illTyped("""
      HNil.head
    """)

    illTyped("""
      HNil.tail
    """)

    illTyped("""
      l.tail.tail.tail.head
    """)
  }

  test("Map") {
    summon[Mapper.Aux[choose.type, HNil, HNil]]
    summon[choose.Case[Set[Int]]]
    summon[Mapper.Aux[choose.type, Set[Int] :: HNil, Option[Int] :: HNil]]

    val s1 = Set(1) :: HNil
    val o1 = s1.map(choose)
    assertTypedEquals[OI](Option(1) :: HNil, o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 = s2.map(choose)
    assertTypedEquals[OIOS](Option(1) :: Option("foo") :: HNil, o2)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val l2 = l1.map(singleton)
    assertTypedEquals[SISSSISI](Set(1) :: Set("foo") :: Set(2) :: Set(3) :: HNil, l2)

    val l3 = l1.map(option)
    assertTypedEquals[OIOSOIOI](Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil, l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil

    val l5 = l4.map(get)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, l5)

    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)

    val l6 = l1.map(id)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, l6)

    val l7 = l4.map(isDefined)
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val l8 = 23 :: "foo" :: true :: HNil
    val l9 = l8.map(mkString)
    assertTypedEquals[String :: String :: String :: HNil]("23" :: "foo" :: "true" :: HNil, l9)

    val l10 = apbp.map(fruit)
    assertTypedEquals[Fruit :: Fruit :: Fruit :: Fruit :: HNil](apbp, l10)

    val l11 = apbp.map(mkString)
    assertTypedEquals[String :: String :: String :: String :: HNil]("Apple()" :: "Pear()" :: "Banana()" :: "Pear()" :: HNil, l11)
  }

  test("Mapped") {
    val meOption = Mapped[HNil, Option]
    val _ = summon[meOption.Out =:= HNil]

    val misOption = Mapped[Int :: String :: HNil, Option]
    val _ = summon[misOption.Out =:= Option[Int] :: Option[String] :: HNil]

    val meId = Mapped[HNil, [a] =>> a]
    val _ = summon[meId.Out =:= HNil]

    val misId = Mapped[Int :: String :: HNil, [a] =>> a]
    val _ = summon[misId.Out =:= Int :: String :: HNil]

    val meConstInt = Mapped[HNil, [a] =>> Int]
    val _ = summon[meConstInt.Out =:= HNil]

    val mdsConstInt = Mapped[Double :: String :: HNil, [a] =>> Int]
    val _ = summon[mdsConstInt.Out =:= Int :: Int :: HNil]
  }

  object dup extends Poly1 {
    given default[T]: Case.Aux[T, T :: T :: HNil] = at[T](t => t :: t :: HNil)
  }

  test("FlatMap") {
    val l1 = 1 :: "foo" :: true :: HNil

    val l2 = l1.flatMap(dup)
    assertTypedEquals[Int :: Int :: String :: String :: Boolean :: Boolean :: HNil](
      1 :: 1 :: "foo" :: "foo" :: true :: true :: HNil, l2)

    val l3 = (1 :: "foo" :: HNil) :: (HNil: HNil) :: (2.0 :: true :: HNil) :: ("bar" :: HNil) :: HNil

    val l4 = l3.flatMap(id)
    assertTypedEquals[Int :: String :: Double :: Boolean :: String :: HNil](
      1 :: "foo" :: 2.0 :: true :: "bar" :: HNil, l4)

    val l5 = 23 :: "foo" :: 7 :: true :: 0 :: HNil
    val l6 = l5.flatMap(incInt)
    assertTypedEquals[Int :: Int :: Int :: HNil](24 :: 8 :: 1 :: HNil, l6)
  }

  test("Conformance") {
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    assertTypedEquals[Any :: AnyRef :: Any :: Any :: HNil](1 :: "foo" :: 2 :: 3 :: HNil, l1)

    val ap = a :: p :: HNil
    typed[AP](ap)
    val bp = b :: p :: HNil
    typed[BP](bp)
    val apap = a :: p :: a :: p :: HNil
    typed[APAP](apap)
    val apbp = a :: p :: b :: p :: HNil
    typed[APBP](apbp)
    val ffff: FFFF = apap
    typed[FFFF](ffff)
  }

  test("Length") {
    val l0 = HNil
    typed[0](l0.length)
    assertEquals(0, l0.length)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    typed[4](l1.length)
    assertEquals(4, l1.length)

    val ap = a :: p :: HNil
    typed[2](ap.length)
    assertEquals(2, ap.length)

    val bp = b :: p :: HNil
    typed[2](bp.length)
    assertEquals(2, bp.length)

    val apap = a :: p :: a :: p :: HNil
    typed[4](apap.length)
    assertEquals(4, apap.length)

    val apbp = a :: p :: b :: p :: HNil
    typed[4](apbp.length)
    assertEquals(4, apbp.length)

    val ffff: FFFF = apap
    typed[4](ffff.length)
    assertEquals(4, ffff.length)
  }

  test("RuntimeLength") {
    assertEquals(0, HNil.runtimeLength)
    assertEquals(1, (123 :: HNil).runtimeLength)
    assertEquals(2, ("abc" :: 123 :: HNil).runtimeLength)
  }

  test("RuntimeList") {
    assertEquals(Nil, HNil.runtimeList)
    assertEquals(List(123), (123 :: HNil).runtimeList)
    assertEquals(List("abc", 123), ("abc" :: 123 :: HNil).runtimeList)
  }

  test("InitLast") {
    val lp = apbp.last
    assertTypedEquals[Pear](p, lp)

    val iapb = apbp.init
    assertTypedEquals[APB](a :: p :: b :: HNil, iapb)
  }

  test("Align") {
    type M0 = Int :: String :: Boolean :: HNil
    type M1 = Int :: Boolean :: String :: HNil
    type M2 = String :: Int :: Boolean :: HNil
    type M3 = String :: Boolean :: Int :: HNil
    type M4 = Boolean :: Int :: String :: HNil
    type M5 = Boolean :: String :: Int :: HNil

    val m0 = 13 :: "bar" :: false :: HNil
    val m1 = 13 :: false :: "bar" :: HNil
    val m2 = "bar" :: 13 :: false :: HNil
    val m3 = "bar" :: false :: 13 :: HNil
    val m4 = false :: 13 :: "bar" :: HNil
    val m5 = false :: "bar" :: 13 :: HNil

    val l = 23 :: "foo" :: true :: HNil

    val a0 = l.align(m0)
    assertTypedEquals[M0](23 :: "foo" :: true :: HNil, a0)

    val a1 = l.align(m1)
    assertTypedEquals[M1](23 :: true :: "foo" :: HNil, a1)

    val a2 = l.align(m2)
    assertTypedEquals[M2]("foo" :: 23 :: true :: HNil, a2)

    val a3 = l.align(m3)
    assertTypedEquals[M3]("foo" :: true :: 23 :: HNil, a3)

    val a4 = l.align(m4)
    assertTypedEquals[M4](true :: 23 :: "foo" :: HNil, a4)

    val a5 = l.align(m5)
    assertTypedEquals[M5](true :: "foo" :: 23 :: HNil, a5)

    val b0 = l.align[M0]
    assertTypedEquals[M0](23 :: "foo" :: true :: HNil, b0)

    val b1 = l.align[M1]
    assertTypedEquals[M1](23 :: true :: "foo" :: HNil, b1)

    val b2 = l.align[M2]
    assertTypedEquals[M2]("foo" :: 23 :: true :: HNil, b2)

    val b3 = l.align[M3]
    assertTypedEquals[M3]("foo" :: true :: 23 :: HNil, b3)

    val b4 = l.align[M4]
    assertTypedEquals[M4](true :: 23 :: "foo" :: HNil, b4)

    val b5 = l.align[M5]
    assertTypedEquals[M5](true :: "foo" :: 23 :: HNil, b5)

    val c0 = (HNil: HNil).align[HNil]
    typed[HNil](c0)

    val c1 = (23 :: HNil).align[Int :: HNil]
    typed[Int :: HNil](c1)

    val c2 = (23 :: "foo" :: HNil).align[String :: Int :: HNil]
    typed[String :: Int :: HNil](c2)

    illTyped("""
      (HNil: HNil).align[Int :: HNil]
    """)

    illTyped("""
      (23 :: HNil).align[String :: HNil]
    """)

    illTyped("""
      (23 :: "foo" :: HNil).align[String :: String :: HNil]
    """)
  }

  test("Reverse") {
    val pbpa = apbp.reverse_
    assertTypedEquals[PBPA](p :: b :: p :: a :: HNil, pbpa)

    val al = a :: HNil
    val ral = al.reverse_
    assertTypedEquals[Apple :: HNil](a :: HNil, ral)
  }

  test("Prepend") {
    val apbp2 = ap ::: bp
    assertTypedEquals[APBP](a :: p :: b :: p :: HNil, apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)

    val pabp = ap reverse_::: bp
    assertTypedEquals[PABP](p :: a :: b :: p :: HNil, pabp)

    {
      // must compile without requiring an implicit Prepend
      def prependWithHNil[L <: HList](list: L) = HNil ::: list
      def prependToHNil[L <: HList](list: L) = list ::: HNil

      val r1 = prependWithHNil(ap)
      assertTypedEquals[AP](ap, r1)
      val r2 = prependToHNil(ap)
      assertTypedEquals[AP](ap, r2)
      val r3 = HNil ::: HNil
      assertTypedEquals[HNil](HNil, r3)

      val r4 = prependWithHNil(pabp)
      assertTypedEquals[PABP](pabp, r4)
      val r5 = prependToHNil(pabp)
      assertTypedEquals[PABP](pabp, r5)
    }

    {
      // must also pass with the default implicit
      val r1 = HNil ::: ap
      assertTypedEquals[AP](ap, r1)
      val r2 = ap ::: HNil
      assertTypedEquals[AP](ap, r2)

      val r4 = HNil ::: pabp
      assertTypedEquals[PABP](pabp, r4)
      val r5 = pabp ::: HNil
      assertTypedEquals[PABP](pabp, r5)
    }

    {
      // must compile without requiring an implicit ReversePrepend
      def reversePrependWithHNil[L <: HList](list: L) = HNil reverse_::: list
      def reversePrependToHNil[L <: HList: Reverse](list: L) = list reverse_::: HNil
      val r4 = reversePrependWithHNil(ap)
      assertTypedEquals[AP](ap, r4)
      val r5 = reversePrependToHNil(ap)
      assertTypedEquals[Pear :: Apple :: HNil](ap.reverse_, r5)
      val r6 = HNil reverse_::: HNil
      assertTypedEquals[HNil](HNil, r6)
    }
  }

  test("Repeat") {
    val ap2 = ap.repeat[2]
    assertTypedEquals[Apple :: Pear :: Apple :: Pear :: HNil](ap2, a :: p :: a :: p :: HNil)

    val ap4 = ap.repeat[4]
    assertTypedEquals[Apple :: Pear :: Apple :: Pear :: Apple :: Pear :: Apple :: Pear :: HNil](
      ap4, a :: p :: a :: p :: a :: p :: a :: p :: HNil
    )

    val ap2_2 = ap2.repeat[2]
    assertTypedEquals[Apple :: Pear :: Apple :: Pear :: Apple :: Pear :: Apple :: Pear :: HNil](ap2_2, ap4)

    {
      // repeating 1 times is identity
      val ap1 = ap.repeat[1]
      assertTypedEquals[AP](ap, ap1)
    }

    {
      // can not repeat zero times
      illTyped("""ap.repeat[0]""")
    }

  }

  test("Unifier") {
    def lub[X, Y, L](x: X, y: Y)(using lb: Lub[X, Y, L]): (L, L) = (lb.left(x), lb.right(y))

    val u21 = lub(a, a)
    typed[(Apple, Apple)](u21)
    val u22 = lub(a, p)
    typed[(Fruit, Fruit)](u22)
    val u23 = lub(a, f)
    typed[(Fruit, Fruit)](u23)
    val u24 = lub(p, a)
    typed[(Fruit, Fruit)](u24)
    val u25 = lub(p, p)
    typed[(Pear, Pear)](u25)
    val u26 = lub(f, f)
    typed[(Fruit, Fruit)](u26)
    val u27 = lub(f, a)
    typed[(Fruit, Fruit)](u27)
    val u28 = lub(f, p)
    typed[(Fruit, Fruit)](u28)
    val u29 = lub(f, f)
    typed[(Fruit, Fruit)](u29)

    summon[Lub[HNil, HNil, HNil]]
    summon[Lub[Apple :: HNil, Apple :: HNil, Apple :: HNil]]
    summon[Lub[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil]]
    summon[Lub[Apple :: Pear :: HNil, Pear :: Apple :: HNil, Fruit :: Fruit :: HNil]]
    summon[Lub[ISII, IIII, IYII]]

    val u31 = lub(HNil, HNil)
    typed[(HNil, HNil)](u31)
    val u32 = lub(a :: HNil, a :: HNil)
    typed[(Apple :: HNil, Apple :: HNil)](u32)
    val u33 = lub(f :: p :: HNil, f :: f :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u33)
    val u34 = lub(a :: p :: HNil, p :: a :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u34)
    val u35 = lub(1 :: "two" :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil)
    typed[(Int :: Any :: Int :: Int :: HNil, Int :: Any :: Int :: Int :: HNil)](u35)

    summon[Unifier.Aux[Apple :: HNil, Apple :: HNil]]
    summon[Unifier.Aux[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    summon[Unifier.Aux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]]

    summon[Unifier.Aux[Int :: String :: Int :: Int :: HNil, YYYY]]

    val uapap = summon[Unifier.Aux[Apple :: Pear :: Apple :: Pear :: HNil, FFFF]]
    val unified1 = uapap(apap)
    typed[FFFF](unified1)
    val unified2 = apap.unify
    typed[FFFF](unified2)

    val ununified1 = Some(unified2).collect {
      case (a1: Apple) :: (p1: Pear) :: (a2: Apple) :: (p2: Pear) :: HNil =>
        a1 :: p1 :: a2 :: p2 :: HNil
    }
    assert(ununified1.isDefined)
    typed[APAP](ununified1.get)
    val ununified2 = Some(unified2).collect {
      case (a: Apple) :: (p1: Pear) :: (b: Banana) :: (p2: Pear) :: HNil =>
        a :: p1 :: b :: p2 :: HNil
    }
    assert(!ununified2.isDefined)
    typed[Option[APBP]](ununified2)

    def getUnifier[L <: HList, Out <: HList](l: L)(using u: Unifier.Aux[L, Out]) = u

    val u2 = getUnifier(a :: HNil)
    typed[Unifier.Aux[Apple :: HNil, Apple :: HNil]](u2)
    val u3 = getUnifier(a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: HNil, Apple :: Apple :: HNil]](u3)
    val u4 = getUnifier(a :: a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: HNil]](u4)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: Apple :: HNil]](u5)
    val u6 = getUnifier(a :: p :: HNil)
    typed[Unifier.Aux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]](u6)
    val u7 = getUnifier(a :: f :: HNil)
    typed[Unifier.Aux[Apple :: Fruit :: HNil, Fruit :: Fruit :: HNil]](u7)
    val u8 = getUnifier(f :: a :: HNil)
    typed[Unifier.Aux[Fruit :: Apple :: HNil, Fruit :: Fruit :: HNil]](u8)
    val u9a = getUnifier(a :: f :: HNil)
    typed[Unifier.Aux[Apple :: Fruit :: HNil, FF]](u9a)
    val u9b = getUnifier(a :: p :: HNil)
    typed[Unifier.Aux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]](u9b)
    val u10 = getUnifier(apap)
    typed[Unifier.Aux[APAP, Fruit :: Fruit :: Fruit :: Fruit :: HNil]](u10)
    val u11 = getUnifier(apbp)
    typed[Unifier.Aux[APBP, Fruit :: Fruit :: Fruit :: Fruit :: HNil]](u11)

    val invar1 = Set(23) :: Set("foo") :: HNil
    val uinvar1 = invar1.unify
    typed[Set[? >: Int & String] :: Set[? >: Int & String] :: HNil](uinvar1)

    // Unifying three or more elements which have an invariant outer type constructor and differing type
    // arguments fails, presumably due to a failure to compute a sensible LUB.
    //val invar2 = Set(23) :: Set("foo") :: Set(true) :: HNil
    //val uinvar2 = invar.unify
  }

  test("SubtypeUnifier") {
    val fruits: Apple :: Pear :: Fruit :: HNil = a :: p :: f :: HNil
    typed[Fruit :: Fruit :: Fruit :: HNil](fruits.unifySubtypes[Fruit])
    typed[Apple :: Pear :: Fruit :: HNil](fruits.unifySubtypes[Apple])
    assertEquals(a :: p :: f :: HNil, fruits.unifySubtypes[Fruit].filter[Fruit])

    val stuff: Apple :: String :: Pear :: HNil = a :: "foo" :: p :: HNil
    typed[Fruit :: String :: Fruit :: HNil](stuff.unifySubtypes[Fruit])
    assertEquals(HNil, stuff.filter[Fruit])
    assertEquals(a :: p :: HNil, stuff.unifySubtypes[Fruit].filter[Fruit])
  }

  test("ToTraversableList") {
    val r1 = HNil.to[List]
    assertTypedEquals[List[Nothing]](Nil, r1)

    ToList[HNil, Nothing]
    ToList[HNil, Int]

    {
      summon[ToTraversable.Aux[M[Int] :: HNil, List, M[Int]]]
      summon[ToTraversable.Aux[M[Int] :: HNil, List, M[?]]]
    }

    val r2 = apap.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, a, p), r2)

    val fruits2 = apbp.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    val stuff = (1 :: "foo" :: 2 :: 3 :: HNil).to[List]
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4.map(isDefined)
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.to[List]
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).to[List]
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(using eq: A =:= B): Unit = {}

    val ctv = cicscicicd.to[List]
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int & String & Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.to[List]
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[? >: Int & String & Double]]](mimsmimimdList, m)

    val mWithEx = mimsmimemd.to[List]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[?]]](mimsmimemdList, mWithEx)

    val m2 = m2im2sm2im2im2d.to[List]
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[? >: Int & String & Double, Unit]]](m2im2sm2im2im2dList, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[List]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[? >: Int & String & Double, ?]]](m2eim2esm2eim2eem2edList, m2e)
  }

  test("ToList") {
    val r1 = HNil.toList
    assertTypedEquals[List[Nothing]](Nil, r1)

    summon[ToTraversable.Aux[HNil, List, Nothing]]
    summon[ToTraversable.Aux[HNil, List, Int]]

    {
      val l1 = (mi :: HNil).toList[M[Int]]
      val l2 = (mi :: HNil).toList[M[?]]

      assertTypedEquals[List[M[Int]]](List(mi), l1)
      assertTypedEquals[List[M[?]]](List(mi), l2)
    }

    val fruits1 = apap.toList
    assertTypedEquals[List[Fruit]](List(a, p, a, p), fruits1)

    val fruits2 = apbp.toList
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toList
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4.map(isDefined)
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toList
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).toList
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(using eq: A =:= B): Unit = {}

    val ctv = cicscicicd.toList
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int & String & Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.toList
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[? >: Int & String & Double]]](mimsmimimdList, m)

    // With existentials, it gets more tricky
    val mWithEx = mimsmimemd.toList
    // Compiler fails complaining that it
    //    Cannot prove that List[HListTests.this.M[? >: Double & _$1 & Int & String]] =:= List[HListTests.this.M[?]]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[?]]](mimsmimemdList, mWithEx)

    // Second order higher kinded types are ok...
    val m2 = m2im2sm2im2im2d.toList
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[? >: Int & String & Double, Unit]]](m2im2sm2im2im2dList, m2)

    // ...as long as existentials are not involved.
    val m2e = m2eim2esm2eim2eem2ed.toList
    // Compiler complains that it
    //    Cannot prove that List[HListTests.this.M2[? >: Double & Int & Int & String & Int, ? >: _$5 & _$3 & _$3 & _$4 & _$3]] =:= List[HListTests.this.M2[35,36] forSome { type _$10; type _$9; type 34 >: _$10 & _$9; type _$8; type _$7; type 32 >: _$8 & _$7; type 35 >: Double & Int & Int & String; type 36 >: _34 & 32 }]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[? >: Int & String & Double, ?]]](m2eim2esm2eim2eem2edList, m2e)
  }

  test("ToTraversableArray") {
    def assertArrayEquals2[T](arr1: Array[T], arr2: Array[T])(using loc: Location): Unit =
      assertEquals(arr1.toList, arr2.toList)

    val empty: Array[Unit] = HNil.toLub[Array, Unit]
    typed[Array[Unit]](empty)
    assertArrayEquals2(Array[Unit](), empty)

    summon[ToTraversable.Aux[HNil, Array, Unit]]
    summon[ToTraversable.Aux[HNil, Array, Int]]

    {
      summon[ToTraversable.Aux[M[Int] :: HNil, Array, M[Int]]]
      summon[ToTraversable.Aux[M[Int] :: HNil, Array, M[?]]]
    }

    val fruits1 = apap.to[Array]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.to[Array]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.to[Array]
    typed[Array[Int | String]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4.map(isDefined)
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2: Array[Boolean] = l7.to[Array]
    typed[Boolean](ll2(0))

    val moreStuff = (a :: "foo" :: p :: HNil).to[Array]
    typed[Array[Apple | String | Pear]](moreStuff)
    assertArrayEquals2(Array[Apple | String | Pear](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(using eq: A =:= B): Unit = {}

    val ctv = cicscicicd.to[Array]
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int & String & Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.to[Array]
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[? >: String & Int & Double <: String | Int | Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.to[Array]
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[?]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.to[Array]
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[? >: String & Int & Double <: String | Int | Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[Array]
    equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    typed[Array[M2[? >: String & Int & Double <: String | Int | Double, ?]]](m2e)
    assertArrayEquals2(m2eim2esm2eim2eem2edArray, m2e)
  }

  test("ToArray") {
    def assertArrayEquals2[T](arr1: Array[T], arr2: Array[T])(using loc: Location): Unit =
      assertEquals(arr1.toList, arr2.toList)

    val empty: Array[Unit] = HNil.toArrayLub
    typed[Array[Unit]](empty)
    assertArrayEquals2(Array[Unit](), empty)

    ToArray[HNil, Unit]
    ToArray[HNil, Int]

    {
      val a1 = (mi :: HNil).toArrayLub[M[Int]]
      val a2 = (mi :: HNil).toArrayLub[M[?]]

      typed[Array[M[Int]]](a1)
      typed[Array[M[?]]](a2)
      assertArrayEquals2(Array[M[Int]](mi), a1)
      assertArrayEquals2(Array[M[?]](mi), a2)
    }

    val fruits1 = apap.toArrayLub[Fruit]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.toArrayLub[Fruit]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toArrayLub
    typed[Array[Int | String]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val ssl = "foo" :: "bar" :: 1L :: HNil
    val ssla = ssl.toArrayLub
    typed[Array[String | Long]](ssla)
    assertArrayEquals2(Array("foo", "bar", 1L), ssla)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4.map(isDefined)
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toArrayLub[Boolean]
    typed[Boolean](ll2(0))

    val moreStuff = (a :: "foo" :: p :: HNil).toArrayLub[AnyRef]
    typed[Array[AnyRef]](moreStuff)
    assertArrayEquals2(Array[AnyRef](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(using eq: A =:= B): Unit = {}

    val ctv = cicscicicd.toArrayLub
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int & String & Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.toArrayLub
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[? >: String & Int & Double <: String | Int | Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.toArrayLub[M[?]]
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[?]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.toArrayLub
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[? >: String & Int & Double <: String | Int | Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.toArrayLub
    equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    typed[Array[M2[? >: String & Int & Double <: String | Int | Double, ?]]](m2e)
    assertArrayEquals2(m2eim2esm2eim2eem2edArray, m2e)
  }

  test("FoldMap") {
    summon[Mapper.Aux[isDefined.type, HNil, HNil]]
    summon[Mapper.Aux[isDefined.type, Option[Int] :: HNil, Boolean :: HNil]]

    val tl1 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val tl2 = Option(1) :: Option("foo") :: (None: Option[Int]) :: Option(3) :: HNil

    val mlfl1 = (tl1.map(isDefined)).toList.foldLeft(true)(_ && _)
    assert(mlfl1)
    val mlfl2 = (tl2.map(isDefined)).toList.foldLeft(true)(_ && _)
    assert(!mlfl2)

    val fl1 = tl1.foldMap(true)(isDefined)(_ && _)
    assert(fl1)
    val fl2 = tl2.foldMap(true)(isDefined)(_ && _)
    assert(!fl2)
  }

  test("At") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val at0 = sn1(0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEqualsDouble(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 ::
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 ::
      20 :: 21 :: 22 :: HNil

    val at22 = sn2(22)
    assertTypedEquals[Int](22, at22)
  }

  test("AtLiteral") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val at0 = sn1(0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEqualsDouble(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 ::
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 ::
      20 :: 21 :: 22 :: HNil

    val at22 = sn2(22)
    assertTypedEquals[Int](22, at22)
  }

  test("TakeDrop") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val r1 = sn1.take(0)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = sn1.drop(0)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r2)

    val r3 = sn1.take(2)
    assertTypedEquals[Int :: Double :: HNil](23 :: 3.0 :: HNil, r3)

    val r4 = sn1.drop(2)
    assertTypedEquals[String :: Unit :: String :: Boolean :: Long :: HNil](
      "foo" :: () :: "bar" :: true :: 5L :: HNil, r4)

    val r5 = sn1.take(7)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r5)

    val r6 = sn1.drop(7)
    assertTypedEquals[HNil](HNil, r6)
  }

  test("TakeDropLiteral") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val r1 = sn1.take(0)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = sn1.drop(0)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r2)

    val r3 = sn1.take(2)
    assertTypedEquals[Int :: Double :: HNil](23 :: 3.0 :: HNil, r3)

    val r4 = sn1.drop(2)
    assertTypedEquals[String :: Unit :: String :: Boolean :: Long :: HNil](
      "foo" :: () :: "bar" :: true :: 5L :: HNil, r4)

    val r5 = sn1.take(7)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r5)

    val r6 = sn1.drop(7)
    assertTypedEquals[HNil](HNil, r6)
  }

  test("Split") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni0)
    val sni1 = sn1.split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int :: Double :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int :: Double :: String :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int :: Double :: String :: Unit :: HNil), (String :: Boolean :: Long :: HNil))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int :: Double :: String :: Unit :: String :: HNil), (Boolean :: Long :: HNil))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: HNil), (Long :: HNil))](sni6)
    val sni7 = sn1.split(7)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil), HNil)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double :: Int :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String :: Double :: Int :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit :: String :: Double :: Int :: HNil), (String :: Boolean :: Long :: HNil))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String :: Unit :: String :: Double :: Int :: HNil), (Boolean :: Long :: HNil))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean :: String :: Unit :: String :: Double :: Int :: HNil), (Long :: HNil))](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil), HNil)](snri7)
  }

  test("SplitLiteral") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni0)
    val sni1 = sn1.split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int :: Double :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int :: Double :: String :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int :: Double :: String :: Unit :: HNil), (String :: Boolean :: Long :: HNil))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int :: Double :: String :: Unit :: String :: HNil), (Boolean :: Long :: HNil))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: HNil), (Long :: HNil))](sni6)
    val sni7 = sn1.split(7)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil), HNil)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double :: Int :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String :: Double :: Int :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit :: String :: Double :: Int :: HNil), (String :: Boolean :: Long :: HNil))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String :: Unit :: String :: Double :: Int :: HNil), (Boolean :: Long :: HNil))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean :: String :: Unit :: String :: Double :: Int :: HNil), (Long :: HNil))](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil), HNil)](snri7)
  }

  test("SplitP") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni0)
    val sni1 = sn1.split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni1)
    val sni2 = sn1.split(2)
    typed[(Int :: Double :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](sni2)
    val sni3 = sn1.split(3)
    typed[(Int :: Double :: String :: HNil, Unit :: String :: Boolean :: Long :: HNil)](sni3)
    val sni4 = sn1.split(4)
    typed[(Int :: Double :: String :: Unit :: HNil, String :: Boolean :: Long :: HNil)](sni4)
    val sni5 = sn1.split(5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil, Boolean :: Long :: HNil)](sni5)
    val sni6 = sn1.split(6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil, Long :: HNil)](sni6)
    val sni7 = sn1.split(7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil, HNil)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[(Double :: Int :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[(String :: Double :: Int :: HNil, Unit :: String :: Boolean :: Long :: HNil)](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[(Unit :: String :: Double :: Int :: HNil, String :: Boolean :: Long :: HNil)](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil, Boolean :: Long :: HNil)](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil, Long :: HNil)](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil, HNil)](snri7)
  }

  test("SplitPLiteral") {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni0)
    val sni1 = sn1.split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni1)
    val sni2 = sn1.split(2)
    typed[(Int :: Double :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](sni2)
    val sni3 = sn1.split(3)
    typed[(Int :: Double :: String :: HNil, Unit :: String :: Boolean :: Long :: HNil)](sni3)
    val sni4 = sn1.split(4)
    typed[(Int :: Double :: String :: Unit :: HNil, String :: Boolean :: Long :: HNil)](sni4)
    val sni5 = sn1.split(5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil, Boolean :: Long :: HNil)](sni5)
    val sni6 = sn1.split(6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil, Long :: HNil)](sni6)
    val sni7 = sn1.split(7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil, HNil)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[(Double :: Int :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[(String :: Double :: Int :: HNil, Unit :: String :: Boolean :: Long :: HNil)](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[(Unit :: String :: Double :: Int :: HNil, String :: Boolean :: Long :: HNil)](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil, Boolean :: Long :: HNil)](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil, Long :: HNil)](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil, HNil)](snri7)
  }

  test("Select") {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val si = sl.select[Int]
    assertTypedEquals[Int](1, si)

    val sb = sl.select[Boolean]
    assertTypedEquals[Boolean](true, sb)

    val ss = sl.select[String]
    assertTypedEquals[String]("foo", ss)

    val sd = sl.select[Double]
    assertEqualsDouble(2.0, sd, Double.MinPositiveValue)
  }

  test("SelectMany") {
    val si = 1 :: true :: "foo" :: 2.0 :: HNil

    val si1 = si.selectManyType[HNil]
    assertTypedEquals[HNil](HNil, si1)

    val si2 = si.selectManyType[0 :: HNil]
    assertTypedEquals[Int :: HNil](1 :: HNil, si2)

    val si3 = si.selectManyType[2 :: HNil]
    assertTypedEquals[String :: HNil]("foo" :: HNil, si3)

    val si4 = si.selectManyType[0 :: 1 :: 2 :: 3 :: HNil]
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 2.0 :: HNil, si4)

    // val si5 = si.selectMany(0 :: HNil)
    // assertTypedEquals[Int :: HNil](1 :: HNil, si5)

    // val si6 = si.selectMany(2 :: HNil)
    // assertTypedEquals[String :: HNil]("foo" :: HNil, si6)

    // val si7 = si.selectMany(0 :: 1 :: 2 :: HNil)
    // assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 2.0 :: HNil, si7)
  }

  test("SelectRange") {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil

    val sl1  = sl.selectRange[0,0]
    val sl1i = sl.selectRange(0,0)
    assertTypedEquals[HNil](HNil, sl1)
    assertTypedEquals[HNil](HNil, sl1i)

    val sl2  = sl.selectRange[1,1]
    val sl2i = sl.selectRange(1,1)
    assertTypedEquals[HNil](HNil, sl2)
    assertTypedEquals[HNil](HNil, sl2i)

    val sl3 = sl.selectRange[0,2]
    val sl3i = sl.selectRange(0,2)
    assertTypedEquals[Int :: Boolean :: HNil](1 :: true :: HNil, sl3)
    assertTypedEquals[Int :: Boolean :: HNil](1 :: true :: HNil, sl3i)

    val sl4 = sl.selectRange[2,4]
    val sl4i = sl.selectRange(2,4)
    assertTypedEquals[String :: Double :: HNil]("foo" :: 2.0 :: HNil, sl4)
    assertTypedEquals[String :: Double :: HNil]("foo" :: 2.0 :: HNil, sl4i)

    val sl5 = sl.selectRange[0,4]
    val sl5i = sl.selectRange(0,4)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 2.0 :: HNil, sl5)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 2.0 :: HNil, sl5i)

  }

  test("Filter") {
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filter[Int]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filter[Int]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  test("FilterNot") {
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filterNot[String]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filterNot[String]
    assertTypedEquals[Int :: Boolean :: Int :: HNil](1 :: true :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  test("Partition") {
    val l1 = 1 :: 2 :: HNil
    val l2 = 1 :: true :: "foo" :: 2 :: HNil

    val r1 = l1.partition[Int]
    assertTypedEquals[(Int :: Int :: HNil, HNil)]((1 :: 2 :: HNil, HNil), r1)

    val r3 = l2.partition[Int]
    assertTypedEquals[(Int :: Int :: HNil, Boolean :: String :: HNil)]((1 :: 2 :: HNil, true :: "foo" :: HNil), r3)
  }

  test("Replace") {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil

    val (i, r1) = sl.replace(23)
    assertTypedEquals[Int](1, i)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](23 :: true :: "foo" :: 2.0 :: HNil, r1)

    val (b, r2) = sl.replace(false)
    assertTypedEquals[Boolean](true, b)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val (s, r3) = sl.replace("bar")
    assertTypedEquals[String]("foo", s)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val (d, r4) = sl.replace(3.0)
    typed[Double](d)
    assertEqualsDouble(2.0, d, Double.MinPositiveValue)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 3.0 :: HNil, r4)

    val (i2, r5) = sl.replaceType[Int]('*')
    typed[Char](r5(0))
    assertTypedEquals[Int](1, i2)
    assertTypedEquals[Char :: Boolean :: String :: Double :: HNil]('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val (b2, r6) = sl.replaceType[Boolean]('*')
    typed[Char](r6(1))
    assertTypedEquals[Boolean](true, b2)
    assertTypedEquals[Int :: Char :: String :: Double :: HNil](1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val (s2, r7) = sl.replaceType[String]('*')
    typed[Char](r7(2))
    assertTypedEquals[String]("foo", s2)
    assertTypedEquals[Int :: Boolean :: Char :: Double :: HNil](1 :: true :: '*' :: 2.0 :: HNil, r7)

    val (d2, r8) = sl.replaceType[Double]('*')
    typed[Double](d2)
    typed[Char](r8(3))
    assertEqualsDouble(2.0, d2, Double.MinPositiveValue)
    assertTypedEquals[Int :: Boolean :: String :: Char :: HNil](1 :: true :: "foo" :: '*' :: HNil, r8)

    val fruits = a :: p :: a :: f :: HNil
    val (x1, rr1) = fruits.replaceType[Pear](a)
    typed[Pear](x1)
    typed[Apple :: Apple :: Apple :: Fruit :: HNil](rr1)

    val (x2, rr2) = fruits.replaceType[Pear](f)
    typed[Pear](x2)
    typed[Apple :: Fruit :: Apple :: Fruit :: HNil](rr2)

    val (x3, rr3) = fruits.replaceType[Fruit](p)
    typed[Fruit](x3)
    typed[Apple :: Pear :: Apple :: Pear :: HNil](rr3)

    val (x4, rr4) = fruits.replace(p)
    typed[Pear](x4)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr4)

    val (x5, rr5) = fruits.replace(f)
    typed[Fruit](x5)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr5)
  }

  test("Update") {
    type SL = Int :: Boolean :: String :: Double :: HNil
    val sl: SL = 1 :: true :: "foo" :: 2.0 :: HNil

    val r1 = sl.updatedElem(23)
    assertTypedEquals[SL](23 :: true :: "foo" :: 2.0 :: HNil, r1)

    val r2 = sl.updatedElem(false)
    assertTypedEquals[SL](1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val r3 = sl.updatedElem("bar")
    assertTypedEquals[SL](1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val r4 = sl.updatedElem(3.0)
    assertTypedEquals[SL](1 :: true :: "foo" :: 3.0 :: HNil, r4)

    val r5 = sl.updatedType[Int]('*')
    assertTypedEquals[Char :: Boolean :: String :: Double :: HNil]('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val r6 = sl.updatedType[Boolean]('*')
    assertTypedEquals[Int :: Char :: String :: Double :: HNil](1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val r7 = sl.updatedType[String]('*')
    assertTypedEquals[Int :: Boolean :: Char :: Double :: HNil](1 :: true :: '*' :: 2.0 :: HNil, r7)

    val r8 = sl.updatedType[Double]('*')
    assertTypedEquals(1 :: true :: "foo" :: '*' :: HNil, r8)

    val r9 = sl.updateTypeWith((i: Int) => i * 2)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](2 :: true :: "foo" :: 2.0 :: HNil, r9)

    val r10 = sl.updateTypeWith((b: Boolean) => !b)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: false :: "foo" :: 2.0 :: HNil, r10)

    val r11 = sl.updateTypeWith((s: String) => s.toUpperCase)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "FOO" :: 2.0 :: HNil, r11)

    val r12 = sl.updateTypeWith((d: Double) => d / 2.0)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 1.0 :: HNil, r12)

    val r13 = sl.updateTypeWith((i: Int) => i.toString)
    assertTypedEquals[String :: Boolean :: String :: Double :: HNil]("1" :: true :: "foo" :: 2.0 :: HNil, r13)

    val r14 = sl.updateTypeWith((b: Boolean) => b.toString)
    assertTypedEquals[Int :: String :: String :: Double :: HNil](1 :: "true" :: "foo" :: 2.0 :: HNil, r14)

    val r15 = sl.updateTypeWith((_: String) => 0xF00)
    assertTypedEquals[Int :: Boolean :: Int :: Double :: HNil](1 :: true :: 0xF00 :: 2.0 :: HNil, r15)

    val r16 = sl.updateTypeWith((d: Double) => d.toString)
    assertTypedEquals[Int :: Boolean :: String :: String :: HNil](1 :: true :: "foo" :: 2.0.toString :: HNil, r16)

    val fruits = a :: p :: a :: f :: HNil

    val rr1 = fruits.updatedType[Pear](a)
    typed[Apple :: Apple :: Apple :: Fruit :: HNil](rr1)

    val rr2 = fruits.updatedType[Pear](f)
    typed[Apple :: Fruit :: Apple :: Fruit :: HNil](rr2)

    val rr3 = fruits.updatedType[Fruit](p)
    typed[Apple :: Pear :: Apple :: Pear :: HNil](rr3)

    val rr4 = fruits.updatedElem(p)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr4)

    val rr5 = fruits.updatedElem(f)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr5)
  }

  test("SplitLeft") {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val (sp1, sp2) = sl.splitLeft[String]
    typed[String :: Double :: HNil](sp2)
    typed[Int :: Boolean :: HNil](sp1)
    assertTypedEquals[SL]((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[Int :: Double :: HNil](sli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](sli2)
    assertTypedEquals[SL2]((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[Boolean :: Int :: HNil](rsp1)
    typed[String :: Double :: HNil](rsp2)
    assertTypedEquals[SL]((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[Double :: Int :: HNil](rsli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](rsli2)
    assertTypedEquals[SL2]((rsli1 reverse_::: rsli2), sl2)
  }

  test("SplitRight") {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val (srp1, srp2) = sl.splitRight[String]
    typed[Int :: Boolean :: String :: HNil](srp1)
    typed[Double :: HNil](srp2)
    assertTypedEquals[SL]((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[Int :: Double :: String :: Unit :: String :: HNil](srli1)
    typed[Boolean :: Long :: HNil](srli2)
    assertTypedEquals[SL2](sl2, srli1 ::: srli2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[String :: Boolean :: Int :: HNil](rsrp1)
    typed[Double :: HNil](rsrp2)
    assertTypedEquals[SL]((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[String :: Unit :: String :: Double :: Int :: HNil](rsrli1)
    typed[Boolean :: Long :: HNil](rsrli2)
    assertTypedEquals[SL2]((rsrli1 reverse_::: rsrli2), sl2)
  }

  test("Transpose") {
    val l1 = 1 :: HNil
    val l2 = ("a" :: HNil) :: HNil

    val r1 = l1.zipOne(l2)
    assertTypedEquals[(Int :: String :: HNil) :: HNil]((1 :: "a" :: HNil) :: HNil, r1)
    val r2 = l1.mapConst(HNil)
    assertTypedEquals[HNil :: HNil](HNil :: HNil, r2)
    val r3 = (l1 :: HNil).transpose
    assertTypedEquals[(Int :: HNil) :: HNil]((1 :: HNil) :: HNil, r3)

    val l3 = 1 :: 2 :: 3 :: HNil
    val l4 = ("a" :: 1.0 :: HNil) :: ("b" :: 2.0 :: HNil) :: ("c" :: 3.0 :: HNil) :: HNil

    type ISD = Int :: String :: Double :: HNil
    val z2 = l3.zipOne(l4)
    assertTypedEquals[ISD :: ISD :: ISD :: HNil](
      (1 :: "a" :: 1.0 :: HNil) :: (2 :: "b" :: 2.0 :: HNil) :: (3 :: "c" :: 3.0 :: HNil) :: HNil, z2
    )

    val r5 = l3.mapConst(HNil)
    assertTypedEquals[HNil :: HNil :: HNil :: HNil](HNil :: HNil :: HNil :: HNil, r5)

    val t2 = l4.transpose
    assertTypedEquals[
      (String :: String :: String :: HNil) ::
      (Double :: Double :: Double :: HNil) ::
      HNil
    ](("a" :: "b" :: "c" :: HNil) :: (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil, t2)

    val t3 = z2.transpose
    assertTypedEquals[
      (Int :: Int :: Int :: HNil) ::
      (String :: String :: String :: HNil) ::
      (Double :: Double :: Double :: HNil) ::
      HNil
    ](
      (1 :: 2 :: 3 :: HNil) ::
      ("a" :: "b" :: "c" :: HNil) ::
      (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil,
      t3
    )

    val r8 = t3.transpose
    assertTypedEquals[ISD :: ISD :: ISD :: HNil](z2, r8)

    val nil: HNil = HNil

    val r9 = nil.zipOne(nil)
    assertTypedEquals[HNil](HNil, r9)

    val r10 = nil.transpose
    assertTypedEquals[HNil](HNil, r10)

    val r11 = (HNil :: HNil :: HNil: HNil :: HNil :: HNil).transpose
    assertTypedEquals[HNil](HNil, r11)

    val r12 = (1 :: HNil).zipOne((2 :: HNil) :: HNil)
    assertTypedEquals[(Int :: Int :: HNil) :: HNil]((1 :: 2 :: HNil) :: HNil, r12)
  }

  test("ZipUnzip") {
    val l1 = 1 :: "a" :: 1.0 :: HNil
    val l2 = 2 :: "b" :: 2.0 :: HNil

    val t1 = (l1 :: l2 :: HNil).transpose
    val z1 = t1.map(tupled)
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z1)

    def zip[L <: HList, OutT <: HList](l : L)(
      using t: Transposer.Aux[L, OutT],
      m: Mapper[tupled.type, OutT],
    ) = l.transpose.map(tupled)

    val z2 = zip(l1 :: l2 :: HNil)
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z2)

    val nil: HNil = HNil
    val z4 = (nil :: nil :: HNil).transpose
    assertTypedEquals[HNil](nil, z4)

    val t2 = z1.map(productElements).transpose
    val u1 = t2.tupled
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u1)

    def unzip[L <: HList, OutM <: HList, OutT <: HList](l: L)(
      using m: Mapper.Aux[productElements.type, L, OutM],
      tr: Transposer.Aux[OutM, OutT],
      tu: Tupler[OutT],
    ) = l.map(productElements).transpose.tupled

    val u2 = unzip(z1)
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u2)

    val r1 = z1.unzip
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), r1)

    val r2 = l1.zip(l2)
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, r2)

    val intInc : Int => Int = _+1
    val stringInc : String => String = _+"*"
    val doubleInc : Double => Int = _.toInt+1

    val l3 = intInc :: stringInc :: doubleInc :: HNil

    val z5 = l3.zipApply(l1)
    assertTypedEquals[Int :: String :: Int :: HNil](2 :: "a*" :: 2 :: HNil, z5)
  }

  test("Unapply") {
    val l = 1 :: true :: "foo" :: 2.0 :: HNil

    val is = l match {
      case i :: true :: s :: 2.0 :: HNil => (i, s)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[Int](1, is._1)
    assertTypedEquals[String]("foo", is._2)

    val is2 = (l: Any) match {
      case (i: Int) :: true :: (s: String) :: 2.0 :: HNil => (i, s)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[Int](1, is2._1)
    assertTypedEquals[String]("foo", is2._2)
  }

  test("Remove") {
    val l = 1 :: true :: "foo" :: HNil

    val li = l.removeElem[Int]
    assertTypedEquals[(Int, Boolean :: String :: HNil)]((1, true :: "foo" :: HNil), li)

    val lb = l.removeElem[Boolean]
    assertTypedEquals[(Boolean, Int :: String :: HNil)]((true, 1 :: "foo" :: HNil), lb)

    val ls = l.removeElem[String]
    assertTypedEquals[(String, Int :: Boolean :: HNil)](("foo", 1 :: true :: HNil), ls)

    val withDuplicates = 1 :: 'a' :: 'b' :: HNil
    val remover = Remove[Int :: Char :: Char :: HNil, Char]
    assertTypedEquals[(Char, Int :: Char :: HNil)](('a', 1 :: 'b' :: HNil), remover(withDuplicates))
  }

  test("RemoveAll") {
    val l = 1 :: true :: "foo" :: HNil

    val lnil = l.removeAll[HNil]
    assertTypedEquals[(HNil, Int :: Boolean :: String :: HNil)]((HNil, 1 :: true :: "foo" :: HNil), lnil)

    val li = l.removeAll[Int :: HNil]
    assertTypedEquals[(Int :: HNil, Boolean :: String :: HNil)]((1 :: HNil, true :: "foo" :: HNil), li)

    val lb = l.removeAll[Boolean :: HNil]
    assertTypedEquals[(Boolean :: HNil, Int :: String :: HNil)]((true :: HNil, 1 :: "foo" :: HNil), lb)

    val lbi = l.removeAll[Boolean :: Int :: HNil]
    assertTypedEquals[(Boolean :: Int :: HNil, String :: HNil)]((true :: 1 :: HNil, "foo" :: HNil), lbi)
  }

  test("Union") {
    type L1 = String :: Long :: HNil
    val l1: L1 = "foo" :: 3L :: HNil

    type L2 = Int :: String :: Boolean :: HNil
    val l2: L2 = 2 :: "bar" :: true :: HNil

    type L3 = Int :: Int :: HNil
    val l3: L3 = 1 :: 2 :: HNil

    type L4 = Int :: Int :: Int :: HNil
    val l4: L4 = 4 :: 5 :: 6 :: HNil

    val lnil = l1.union[HNil](HNil)
    assertTypedEquals[L1](l1, lnil)

    val lself = l1.union(l1)
    assertTypedEquals[L1](l1, lself)

    val l12 = l1.union(l2)
    assertTypedEquals[String :: Long :: Int :: Boolean :: HNil]("foo" :: 3L :: 2 :: true :: HNil, l12)

    val l21 = l2.union(l1)
    assertTypedEquals[Int :: String :: Boolean :: Long :: HNil](2 :: "bar" :: true :: 3L :: HNil, l21)


    illTyped { """summon[Union.Aux[Int :: HNil, Int :: HNil, Int :: Int :: HNil]]"""}

    val ldup1 = (l3).union(l4)
    assertTypedEquals[Int :: Int :: Int :: HNil](1 :: 2 :: 6 :: HNil, ldup1)

    val ldup2 = (l4).union(l3)
    assertTypedEquals[Int :: Int :: Int :: HNil](4 :: 5 :: 6 :: HNil, ldup2)
  }

  test("Intersection") {
    type L1 = String :: Long :: Int :: HNil
    val l1: L1 = "foo" :: 1L :: 3 :: HNil

    type L2 = Int :: String :: Boolean :: HNil
    val l2: L2 = 2 :: "bar" :: true :: HNil

    type L3 = Int :: String :: Int :: HNil
    val l3: L3 = 4 :: "foo" :: 5 :: HNil

    val lnil = l1.intersect[HNil]
    assertTypedEquals[HNil](HNil, lnil)

    val lself = l1.intersect[L1]
    assertTypedEquals[L1](l1, lself)

    val l12 = l1.intersect[L2]
    assertTypedEquals[String :: Int :: HNil]("foo" :: 3 :: HNil, l12)

    val l21 = l2.intersect[L1]
    assertTypedEquals[Int :: String :: HNil](2 :: "bar" :: HNil, l21)

    illTyped { """summon[Intersection.Aux[Int :: HNil, Int :: HNil, HNil]]"""}

    val ldup1 = (l3).intersect[Int :: HNil]
    assertTypedEquals[Int :: HNil](4 :: HNil, ldup1)

    val ldup2 = (l3).intersect[Int :: Int :: HNil]
    assertTypedEquals[Int :: Int :: HNil](4 :: 5 :: HNil, ldup2)

    val ldup3 = (l3).intersect[String :: HNil]
    assertTypedEquals[String :: HNil]("foo" :: HNil, ldup3)
  }

  test("Diff") {
    type L1 = String :: Long :: Int :: HNil
    val l1: L1 = "foo" :: 1L :: 3 :: HNil

    type L2 = Int :: String :: Boolean :: HNil
    val l2: L2 = 2 :: "bar" :: true :: HNil

    type L3 = Int :: Boolean :: Int :: HNil
    val l3: L3 = 4 :: false :: 5 :: HNil

    val lnil = l1.diff[HNil]
    assertTypedEquals[L1](l1, lnil)

    val lself = l1.diff[L1]
    assertTypedEquals[HNil](HNil, lself)

    val l12 = l1.diff[L2]
    assertTypedEquals[Long :: HNil](1L :: HNil, l12)

    val l21 = l2.diff[L1]
    assertTypedEquals[Boolean :: HNil](true :: HNil, l21)

    val ldup1 = (l3).diff[Int :: HNil]
    assertTypedEquals[Boolean :: Int :: HNil](false :: 5 :: HNil, ldup1)

    val ldup2 = (l3).diff[Int :: Int :: HNil]
    assertTypedEquals[Boolean :: HNil](false :: HNil, ldup2)

    val ldup3 = (l3).diff[Boolean :: HNil]
    assertTypedEquals[Int :: Int :: HNil](4 :: 5 :: HNil, ldup3)
  }

  test("Reinsert") {
    type L = Int :: Boolean :: String :: HNil

    val l: L = 1 :: true :: "foo" :: HNil

    val (i, li) = l.removeElem[Int]
    assertTypedEquals[L](li.reinsert[L](i), l)

    val (b, lb) = l.removeElem[Boolean]
    assertTypedEquals[L](lb.reinsert[L](b), l)

    val (s, ls) = l.removeElem[String]
    assertTypedEquals[L](ls.reinsert[L](s), l)
  }

  test("ReinsertAll") {
    type L = Int :: Boolean :: String :: HNil

    val l = 1 :: true :: "foo" :: HNil

    val (nil, lnil) = l.removeAll[HNil]
    assertTypedEquals[L](lnil.reinsertAll[L](nil), l)

    val (i, li) = l.removeAll[Int :: HNil]
    assertTypedEquals[L](li.reinsertAll[L](i), l)

    val (b, lb) = l.removeAll[Boolean :: HNil]
    assertTypedEquals[L](lb.reinsertAll[L](b), l)

    val (bi, lbi) = l.removeAll[Boolean :: Int :: HNil]
    assertTypedEquals[L](lbi.reinsertAll[L](bi), l)
  }

  object combine extends Poly2 {
    given caseCharString: Case.Aux[Char, String, Int] = at((c: Char, s: String) => s.indexOf(c))
    given caseIntBoolean: Case.Aux[Int, Boolean, String] = at((i: Int, b: Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

  test("FoldLeft") {
    val c1a = combine('o', "foo")
    val c1b = combine(c1a, true)
    assertTypedEquals[String]("pass", c1b)

    summon[LeftFolder.Aux[HNil, String, combine.type, String]]
    summon[LeftFolder.Aux[Boolean :: HNil, Int, combine.type, String]]
    summon[LeftFolder.Aux[String :: Boolean :: HNil, Char, combine.type, String]]

    val _ = summon[LeftFolder[HNil, String, combine.type]]
    val _ = summon[LeftFolder[Boolean :: HNil, Int, combine.type]]
    val _ = summon[LeftFolder[String :: Boolean :: HNil, Char, combine.type]]

    val l1 = "foo" :: true :: HNil
    val f1 = l1.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f1)

    val c2a = combine('o', "bar")
    val c2b = combine(c2a, false)
    assertTypedEquals[String]("pass", c2b)

    val l2 = "bar" :: false :: HNil
    val f2 = l2.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f2)
  }

  test("UpdatedAt") {
    type IBS = Int :: Boolean :: String :: HNil
    val l = 1 :: true :: "foo" :: HNil

    val r1 = l.updatedAt[0](2)
    assertTypedEquals[IBS](2 ::  true :: "foo" :: HNil, r1)

    val r2 = l.updatedAt[1](false)
    assertTypedEquals[IBS](1 :: false :: "foo" :: HNil, r2)

    val r3 = l.updatedAt[2]("bar")
    assertTypedEquals[IBS](1 ::  true :: "bar" :: HNil, r3)
  }

  test("UpdatedAtLiteral") {
    type IBS = Int :: Boolean :: String :: HNil
    val l = 1 :: true :: "foo" :: HNil

    val r1 = l.updatedAt(0, 2)
    assertTypedEquals[IBS](2 ::  true :: "foo" :: HNil, r1)

    val r2 = l.updatedAt(1, false)
    assertTypedEquals[IBS](1 :: false :: "foo" :: HNil, r2)

    val r3 = l.updatedAt(2, "bar")
    assertTypedEquals[IBS](1 ::  true :: "bar" :: HNil, r3)
  }

  test("ZipConst") {
    type IBS = Int :: Boolean :: String :: HNil
    val c = 5
    type WithConst = (Int, Int) :: (Boolean, Int) :: (String, Int) :: HNil
    val l = 1 :: true :: "a" :: HNil
    typed[IBS](l)
    val expected = (1, c) :: (true, c) :: ("a", c) :: HNil

    val zcIntIbs = ZipConst[Int, IBS]
    val zipped1 = zcIntIbs(c, l)
    assertTypedEquals[WithConst](expected, zipped1)

    val zcaIntIbs = ZipConst[Int, IBS]
    assertTypedEquals[WithConst](expected, zcaIntIbs(c, l))

    val x = l.zipConst(c)
    assertTypedEquals[WithConst](expected, x)

    assertTypedEquals[HNil](HNil, HList().zipConst(""))
  }

  test("ZipWith") {
    object empty extends Poly2

    object add extends Poly2 {
      given caseIntInt: Case.Aux[Int, Int, Int] = at[Int, Int](_ + _)
    }

    // HNil zipWith HNil (emptyFn)
    val r1 = (HNil: HNil).zipWith(HNil: HNil)(empty)
    assertTypedEquals[HNil](HNil, r1)

    // HNil zipWith nonEmpty (emptyFn)
    val r2 = (HNil: HNil).zipWith(1 :: HNil)(empty)
    assertTypedEquals[HNil](HNil, r2)

    // nonEmpty zipWith HNil (emptyFn)
    val r3 = (1 :: HNil).zipWith(HNil: HNil)(empty)
    assertTypedEquals[HNil](HNil, r3)

    // singleton zipWith singleton
    val r4 = (1 :: HNil).zipWith(2 :: HNil)(add)
    assertTypedEquals[Int :: HNil](3 :: HNil, r4)

    { // longList zipWith longerList
      type Left  = Int :: String :: Double :: HNil
      type Right = Int :: Double :: String :: Boolean :: HNil

      val left: Left   = 1 :: "foo" :: 1.2 :: HNil
      val right: Right = 2 :: 2.3 :: "3.4" :: true :: HNil

      object zipFn extends Poly2 {
        given caseIntInt: Case.Aux[Int, Int, Int] = at[Int, Int](_ + _)
        given caseStringDouble: Case.Aux[String, Double, String] = at[String, Double](_ + " -> " + _.toString)
        given caseDoubleString: Case.Aux[Double, String, Double] = at[Double, String](_ + _.toDouble)
      }

      val r5 = left.zipWith(right)(zipFn)
      assertTypedEquals[Int :: String :: Double :: HNil](3 :: "foo -> 2.3" :: 4.6 :: HNil, r5)
    }

    { // invalid polys
      illTyped("""
        (1 :: HNil).zipWith(2 :: HNil)(empty)
      """)

      object noIntFn extends Poly2 {
        given caseDoubleDouble: Case.Aux[Double, Double, Double] = at[Double, Double](_ + _)
      }

      illTyped("""
        (1 :: HNil).zipWith(2 :: HNil)(noIntFn)
      """)

      illTyped("""
        (1.0 :: 2 :: HNil).zipWith(2.0 :: 3 :: HNil)(noIntFn)
      """)
    }
  }

  test("ZipWithIndex") {
    // HNil zipWithIndex
    val r1 = (HNil: HNil).zipWithIndex
    assertTypedEquals[HNil](HNil, r1)

    // One element HList zipWithIndex
    val r2 = (0 :: HNil).zipWithIndex
    assertTypedEquals[(Int, 0) :: HNil]((0, 0) :: HNil, r2)

    // HList zipWithIndex
    val r3 = (0 :: 1 :: 2 :: 3 :: HNil).zipWithIndex
    type ZWI = (Int, 0) :: (Int, 1) :: (Int, 2) :: (Int, 3) :: HNil
    assertTypedEquals[ZWI](
      ((0, 0) :: (1, 1) :: (2, 2) :: (3, 3) :: HNil).asInstanceOf[ZWI],
      r3,
    )
  }

  test("WithKeys") {
    import formless.record.*

    val orig =
      ("intField" ->> 1) ::
      ("boolField" ->> true) ::
      HNil

    val result = orig.values.zipWithKeys(orig.keys)
    sameTyped(orig)(result)
    assertEquals(orig, result)
    val int = result.get("intField")
    assertTypedEquals[Int](1, int)
    val bool = result.get("boolField")
    assertTypedEquals[Boolean](true, bool)
    illTyped("""result.get("otherField")""")

    // key/value lengths must match up
    illTyped("orig.tail.values.zipWithKeys(orig.keys)")
    illTyped("orig.values.zipWithKeys(orig.keys.tail)")

    // Explicit type argument
    {
      val result = orig.values.zipWithKeys["intField" :: "boolField" :: HNil]
      sameTyped(orig)(result)
      assertEquals(orig, result)
      val int = result.get("intField")
      assertTypedEquals[Int](1, int)
      val bool = result.get("boolField")
      assertTypedEquals[Boolean](true, bool)
      illTyped("""result.get("otherField")""")

      // key/value lengths must match up
      illTyped(""" orig.tail.values.zipWithKeys["intField" :: "boolField" :: HNil] """)
      illTyped(""" orig.values.zipWithKeys["boolField" :: HNil] """)
    }
  }

  test("Collect") {
    object empty extends Poly1

    object complex extends Poly1 {
      given caseInt: Case.Aux[Int, Double] = at[Int](_.toDouble)
      given caseString: Case.Aux[String, Int] = at[String](_ => 1)
    }

    val in: Int :: String :: Double :: HNil = 1 :: "foo" :: 2.2 :: HNil

    // HNil collect p
    val r1 = (HNil: HNil).collect(empty)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = (HNil: HNil).collect(id)
    assertTypedEquals[HNil](HNil, r2)

    val r3 = (HNil: HNil).collect(complex)
    assertTypedEquals[HNil](HNil, r3)

    // non-HNil collect empty
    val r4 = in.collect(empty)
    assertTypedEquals[HNil](HNil, r4)

    // non-HNil collect identity
    val r5 = in.collect(id)
    assertTypedEquals[Int :: String :: Double :: HNil](in, r5)

    // non-HNil collect complex
    val r6 = in.collect(complex)
    assertTypedEquals[Double :: Int :: HNil](1.0 :: 1 :: HNil, r6)
  }

  // @Test
  // def testOrdering: Unit = {
  //   assertEquals(List(HNil: HNil, HNil), List(HNil: HNil, HNil).sorted)

  //   assertEquals(List(1 :: HNil, 2 :: HNil, 3 :: HNil), List(2 :: HNil, 1 :: HNil, 3 :: HNil).sorted)

  //   assertEquals(
  //     List(1 :: "abc" :: HNil, 1 :: "def" :: HNil, 2 :: "abc" :: HNil, 2 :: "def" :: HNil),
  //     List(2 :: "abc" :: HNil, 1 :: "def" :: HNil, 2 :: "def" :: HNil, 1 :: "abc" :: HNil).sorted
  //   )
  // }

  test("MapCons") {
    type C = Char; type S = String; type I = Int; type D = Double

    val r1 = (HNil: HNil).mapCons('a')
    assertTypedEquals[HNil](HNil, r1)

    val r2 = (HNil :: HNil).mapCons('a')
    assertTypedEquals[(Char :: HNil) :: HNil]((('a' :: HNil) :: HNil), r2)

    val r3 = ((1 :: HNil) :: ("foo" :: HNil) :: (2.0 :: HNil) :: HNil).mapCons('a')
    assertTypedEquals[(C :: I :: HNil) :: (C :: S :: HNil) :: (C :: D :: HNil) :: HNil](
      ('a' :: 1 :: HNil) :: ('a' :: "foo" :: HNil) :: ('a' :: 2.0 :: HNil) :: HNil,
      r3
    )
  }

  test("Interleave") {
    type C = Char; type S = String; type I = Int; type D = Double
    def interleave[I, L <: HList](i: I, l: L)(using interleave: Interleave[I, L]): interleave.Out = interleave(i, l)

    val r1 = interleave('i', HNil)
    assertTypedEquals[(Char :: HNil) :: HNil](('i' :: HNil) :: HNil, r1)

    val r2 = interleave('i', 1 :: HNil)
    assertTypedEquals[(C :: I :: HNil) :: (I :: C :: HNil) :: HNil](('i' :: 1 :: HNil) :: (1 :: 'i' :: HNil) :: HNil,
      r2
    )

    val r3 = interleave('i', 1 :: "foo" :: HNil)
    assertTypedEquals[(C :: I :: S :: HNil) :: (I :: C :: S :: HNil) :: (I :: S :: C :: HNil) :: HNil](
      ('i' :: 1 :: "foo" :: HNil) ::
      (1 :: 'i' :: "foo" :: HNil) ::
      (1 :: "foo" :: 'i' :: HNil) ::
      HNil,
      r3
    )

    val r4 = interleave('i', 1 :: "foo" :: 2.0 :: HNil)
    assertTypedEquals[(C :: I :: S :: D :: HNil) :: (I :: C :: S :: D :: HNil) :: (I :: S :: C :: D :: HNil) :: (I :: S :: D :: C :: HNil) :: HNil](
      ('i' :: 1 :: "foo" :: 2.0 :: HNil) ::
      (1 :: 'i' :: "foo" :: 2.0 :: HNil) ::
      (1 :: "foo" :: 'i' :: 2.0 :: HNil) ::
      (1 :: "foo" :: 2.0 :: 'i' :: HNil) ::
      HNil,
      r4
    )
  }

  test("FlatMapInterleave") {
    type C = Char; type I = Int

    def flatMapInterleave[I, L <: HList](i: I, l: L)(using flatMapInterleave: FlatMapInterleave[I, L]) =
      flatMapInterleave(i, l)

    val r1 = flatMapInterleave('i', HNil)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = flatMapInterleave('i', HNil :: HNil)
    assertTypedEquals[(Char :: HNil) :: HNil](('i' :: HNil) :: HNil, r2)

    val r3 = flatMapInterleave('i', (1 :: HNil) :: (2 :: HNil) :: HNil)
    assertTypedEquals[(C :: I :: HNil) :: (I :: C :: HNil) :: (C :: I :: HNil) :: (I :: C :: HNil) :: HNil](
      ('i' :: 1 :: HNil) ::
      (1 :: 'i' :: HNil) ::
      ('i' :: 2 :: HNil) ::
      (2 :: 'i' :: HNil) ::
      HNil,
      r3
    )
  }

  test("Permutations") {
    type S = String; type I = Int; type D = Double

    val r1 = HNil.permutations
    assertTypedEquals[HNil :: HNil](HNil :: HNil, r1)

    val r2 = (1 :: HNil).permutations
    assertTypedEquals[(Int :: HNil) :: HNil]((1 :: HNil) :: HNil, r2)

    val r3 = (1 :: "foo" :: HNil).permutations
    assertTypedEquals[(I :: S :: HNil) :: (S :: I :: HNil) :: HNil](
      (1 :: "foo" :: HNil) ::
      ("foo" :: 1 :: HNil) ::
      HNil,
      r3
    )

    val r4 = (1 :: "foo" :: 2.0 :: HNil).permutations
    assertTypedEquals[
      (I :: S :: D :: HNil) :: (S :: I :: D :: HNil) :: (S :: D :: I :: HNil) ::
      (I :: D :: S :: HNil) :: (D :: I :: S :: HNil) :: (D :: S :: I :: HNil) :: HNil
    ](
      (1 :: "foo" :: 2.0 :: HNil) ::
      ("foo" :: 1 :: 2.0 :: HNil) ::
      ("foo" :: 2.0 :: 1 :: HNil) ::
      (1 :: 2.0 :: "foo" :: HNil) ::
      (2.0 :: 1 :: "foo" :: HNil) ::
      (2.0 :: "foo" :: 1 :: HNil) ::
      HNil,
      r4
    )
  }

  test("MkString") {
    assertEquals(s"⸨1, foo, ${2.0}⸩", (1 :: "foo" :: 2.0 :: HNil).mkString("⸨", ", ", "⸩"))
  }

  test("RotateLeft") {
    val in0 = HNil
    val in1 = 1 :: HNil
    val in2 = 1 :: "foo" :: HNil
    val in3 = 1 :: "foo" :: 2.0 :: HNil
    val in4 = 1 :: "foo" :: 2.0 :: 'a' :: HNil
    type S = String
    type I = Int
    type D = Double
    type C = Char

    { // rotateLeft(0)
      val r1 = in0.rotateLeft(0)
      assertTypedEquals[HNil](HNil, r1)
      val r2 = in1.rotateLeft(0)
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft(0)
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateLeft(0)
      assertTypedEquals[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateLeft(0)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateLeft[0]
      val r1 = in0.rotateLeft[0]
      assertTypedEquals[HNil](HNil, r1)
      val r2 = in1.rotateLeft[0]
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft[0]
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateLeft[0]
      assertTypedEquals[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateLeft[0]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateLeft(n % size == 0)
      val r1 = in1.rotateLeft(1)
      assertTypedEquals[Int :: HNil](in1, r1)
      val r2 = in1.rotateLeft(2)
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft(2)
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateLeft(4)
      assertTypedEquals[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateLeft(3)
      assertTypedEquals[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateLeft(6)
      assertTypedEquals[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateLeft(4)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateLeft(8)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // rotateLeft[N % Size == 0]
    val r1 = in1.rotateLeft[1]
      assertTypedEquals[I :: HNil](in1, r1)
      val r2 = in1.rotateLeft[2]
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft[2]
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateLeft[4]
      assertTypedEquals[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateLeft[3]
      assertTypedEquals[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateLeft[6]
      assertTypedEquals[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateLeft[4]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateLeft[8]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // other(n)
      val r1 = in2.rotateLeft(1)
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateLeft(1)
      assertTypedEquals[S :: D :: I :: HNil]("foo" :: 2.0 :: 1 :: HNil, r2)

      val r3 = in4.rotateLeft(1)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r3)

      val r4 = in4.rotateLeft(2)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateLeft(3)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r5)

      val r6 = in4.rotateLeft(5)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r6)

      val r7 = in4.rotateLeft(6)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }

    { // other[N]
    val r1 = in2.rotateLeft[1]
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateLeft[1]
      assertTypedEquals[S :: D :: I :: HNil]("foo" :: 2.0 :: 1 :: HNil, r2)

      val r3 = in4.rotateLeft[1]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r3)

      val r4 = in4.rotateLeft[2]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateLeft[3]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r5)

      val r6 = in4.rotateLeft[5]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r6)

      val r7 = in4.rotateLeft[6]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }
  }

  test("RotateRight") {
    val in0 = HNil
    val in1 = 1 :: HNil
    val in2 = 1 :: "foo" :: HNil
    val in3 = 1 :: "foo" :: 2.0 :: HNil
    val in4 = 1 :: "foo" :: 2.0 :: 'a' :: HNil
    type S = String; type I = Int; type D = Double; type C = Char

    { // rotateRight(0)
      val r1 = in0.rotateRight(0)
      assertTypedEquals[HNil](HNil, r1)
      val r2 = in1.rotateRight(0)
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateRight(0)
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateRight(0)
      assertTypedEquals[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateRight(0)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateRight[0]
      val r1 = in0.rotateRight[0]
      assertTypedEquals[HNil](HNil, r1)
      val r2 = in1.rotateRight[0]
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateRight[0]
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateRight[0]
      assertTypedEquals[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateRight[0]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateRight(n % size == 0)
      val r1 = in1.rotateRight(1)
      assertTypedEquals[I :: HNil](in1, r1)
      val r2 = in1.rotateRight(2)
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateRight(2)
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateRight(4)
      assertTypedEquals[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateRight(3)
      assertTypedEquals[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateRight(6)
      assertTypedEquals[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateRight(4)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateRight(8)
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // rotateRight[N % Size == 0]
      val r1 = in1.rotateRight[1]
      assertTypedEquals[I :: HNil](in1, r1)
      val r2 = in1.rotateRight[2]
      assertTypedEquals[I :: HNil](in1, r2)
      val r3 = in2.rotateRight[2]
      assertTypedEquals[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateRight[4]
      assertTypedEquals[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateRight[3]
      assertTypedEquals[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateRight[6]
      assertTypedEquals[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateRight[4]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateRight[8]
      assertTypedEquals[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // others(n)
      val r1 = in2.rotateRight(1)
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateRight(1)
      assertTypedEquals[D :: I :: S :: HNil](2.0 :: 1 :: "foo" :: HNil, r2)

      val r3 = in4.rotateRight(1)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r3)

      val r4 = in4.rotateRight(2)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateRight(3)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r5)

      val r6 = in4.rotateRight(5)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r6)

      val r7 = in4.rotateRight(6)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }

    { // others[N]
      val r1 = in2.rotateRight[1]
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateRight[1]
      assertTypedEquals[D :: I :: S :: HNil](2.0 :: 1 :: "foo" :: HNil, r2)

      val r3 = in4.rotateRight[1]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r3)

      val r4 = in4.rotateRight[2]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateRight[3]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r5)

      val r6 = in4.rotateRight[5]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r6)

      val r7 = in4.rotateRight[6]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }
  }

  object smear extends Poly2 {
    given caseIntInt: Case.Aux[Int, Int, Int] = at((x: Int, y: Int) => x + y)
    given caseStringInt: Case.Aux[String, Int, Int] = at((x: String, y: Int) => x.toInt + y)
    given caseIntString: Case.Aux[Int, String, Int] = at((x: Int, y: String) => x + y.toInt)
  }

  test("ScanLeft") {
    val in = 1 :: "2" :: HNil
    val out = in.scanLeft(1)(smear)

    typed[Int :: Int :: Int :: HNil](out)
    assertEquals(1 :: 2 :: 4 :: HNil, out)
  }

  test("ScanRight") {
    val in = 1 :: "2" :: HNil
    val out = in.scanRight(1)(smear)

    typed[Int :: Int :: Int :: HNil](out)
    assertEquals(4 :: 3 :: 1 :: HNil, out)
  }

  test("Fill") {
    {
      val empty = Fill[0](true)
      typed[0](empty.length)
    }

    {
      val empty = Fill[0, Boolean](true)
      typed[0](empty.length)
    }

    {
      val single = Fill[1](None)
      typed[1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val single = Fill[1, None.type](None)
      typed[1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val three = Fill[3](m2i)
      typed[3](three.length)
      typed[M2[Int, Unit]](three(0))
      typed[M2[Int, Unit]](three(1))
      typed[M2[Int, Unit]](three(2))
      assertEquals(m2i, three(0))
      assertEquals(m2i, three(1))
      assertEquals(m2i, three(2))
    }

    {
      val three = Fill[3, M2[Int, Unit]](m2i)
      typed[3](three.length)
      typed[M2[Int, Unit]](three(0))
      typed[M2[Int, Unit]](three(1))
      typed[M2[Int, Unit]](three(2))
      assertEquals(m2i, three(0))
      assertEquals(m2i, three(1))
      assertEquals(m2i, three(2))
    }

    {
      val empty = Fill[(0, 0)](true)
      typed[0](empty.length)
    }

    {
      val empty = Fill[(0, 0), Boolean](true)
      typed[0](empty.length)
    }

    {
      val empty = Fill[(2, 0)](true)
      typed[2](empty.length)
      typed[0](empty(0).length)
      typed[0](empty(1).length)
    }

    {
      val empty = Fill[(2, 0), Boolean](true)
      typed[2](empty.length)
      typed[0](empty(0).length)
      typed[0](empty(1).length)
    }

    {
      val empty = Fill[(0, 2)](true)
      typed[0](empty.length)
    }

    {
      val empty = Fill[(0, 2), Boolean](true)
      typed[0](empty.length)
    }

    {
      val oneByTwo = Fill[(1, 2)](None)
      typed[1](oneByTwo.length)
      typed[2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(0))
      typed[None.type](oneByTwo.head(1))
      assertEquals(None, oneByTwo.head(0))
      assertEquals(None, oneByTwo.head(1))
    }

    {
      val oneByTwo = Fill[(1, 2), None.type](None)
      typed[1](oneByTwo.length)
      typed[2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(0))
      typed[None.type](oneByTwo.head(1))
      assertEquals(None, oneByTwo.head(0))
      assertEquals(None, oneByTwo.head(1))
    }

    {
      val twoByThree = Fill[(2, 3)](None)
      typed[2](twoByThree.length)
      typed[3](twoByThree(0).length)
      typed[3](twoByThree(1).length)
      typed[None.type](twoByThree.at[0].at[0])
      typed[None.type](twoByThree.at[0].at[1])
      typed[None.type](twoByThree.at[0].at[2])
      typed[None.type](twoByThree.at[1].at[0])
      typed[None.type](twoByThree.at[1].at[1])
      typed[None.type](twoByThree.at[1].at[2])
      assertEquals(None, twoByThree.at[0].at[0])
      assertEquals(None, twoByThree.at[0].at[1])
      assertEquals(None, twoByThree.at[0].at[2])
      assertEquals(None, twoByThree.at[1].at[0])
      assertEquals(None, twoByThree.at[1].at[1])
      assertEquals(None, twoByThree.at[1].at[2])
    }

    {
      val twoByThree = Fill[(2, 3), None.type](None)
      typed[2](twoByThree.length)
      typed[3](twoByThree(0).length)
      typed[3](twoByThree(1).length)
      typed[None.type](twoByThree.at[0].at[0])
      typed[None.type](twoByThree.at[0].at[1])
      typed[None.type](twoByThree.at[0].at[2])
      typed[None.type](twoByThree.at[1].at[0])
      typed[None.type](twoByThree.at[1].at[1])
      typed[None.type](twoByThree.at[1].at[2])
      assertEquals(None, twoByThree.at[0].at[0])
      assertEquals(None, twoByThree.at[0].at[1])
      assertEquals(None, twoByThree.at[0].at[2])
      assertEquals(None, twoByThree.at[1].at[0])
      assertEquals(None, twoByThree.at[1].at[1])
      assertEquals(None, twoByThree.at[1].at[2])
    }
  }

  test("PolyFill") {
    object zero extends Poly0 {
      given zeroInt: Case0[Int] = at[Int](0)
    }

    given emptyString: zero.Case0[String] = zero.at[String]("")

    val out = FillWith[zero.type, Int :: String :: Int :: HNil]()
    assertEquals(out, 0 :: "" :: 0 :: HNil)
  }

  test("Patch") {
    val basehl = 1 :: 2 :: "three" :: HNil

    { //patch an empty hlist
      val out = HNil.patch(0, basehl, 0)
      val out2 = HNil.patch[0,0](basehl)

      typed[Int :: Int :: String :: HNil](out)
      assertEquals(out, basehl)
      assertTypedEquals[Int :: Int :: String :: HNil](out, out2)
    }

    { //single patch w/ nothing removed
      val out = basehl.patch(1, 4 :: HNil, 0)
      val out2 = basehl.patch[1,0](4 :: HNil)

      typed[Int :: Int :: Int :: String :: HNil](out)
      assertEquals(1 :: 4 :: 2 :: "three" :: HNil, out)
      assertTypedEquals[Int :: Int :: Int :: String :: HNil](out, out2)
    }

    { //single patch w/ 2 elements removed
      val out = basehl.patch(1, 3 :: HNil, 2)
      val out2 = basehl.patch[1,2](3 :: HNil)

      typed[Int :: Int :: HNil](out)
      assertEquals(1 :: 3 :: HNil, out)
      assertTypedEquals[Int :: Int :: HNil](out, out2)
    }

    { //essentially append
      val p = 4 :: 5 :: "six" :: HNil
      val out = basehl.patch(3, p, 0)
      val out2 = basehl.patch[3,0](p)

      typed[Int :: Int :: String :: Int :: Int :: String :: HNil](out)
      assertEquals(1 :: 2 :: "three" :: 4 :: 5 :: "six" :: HNil, out)
      assertTypedEquals[Int :: Int :: String :: Int :: Int :: String :: HNil](out, out2)
    }

    { //several patched w/ everything from original removed
      val sub = 4 :: "five" :: "six" :: HNil
      val out = basehl.patch(0, sub, 3)
      val out2 = basehl.patch[0,3](sub)

      typed[Int :: String :: String :: HNil](out)
      assertEquals(sub, out)
      assertTypedEquals[Int :: String :: String :: HNil](out, out2)
    }
  }

  // @Test // TODO
  // def testToEither: Unit = {
  //   type PISB = Int :: String :: Boolean :: HNil
  //   type CISBa = Int :+: String :+: Boolean :+: CNil
  //   type CISBb = the.`ToCoproduct[PISB]`.Out
  //   summon[CISBa =:= CISBb]
  // }

  // @Test
  // def testToSum: Unit = {
  //   type PISB = Int :: String :: Boolean :: HNil
  //   type CISBa = Int :+: String :+: Boolean :+: CNil
  //   type SISBa = the.`ToSum[PISB]`.Out
  //   summon[CISBa =:= SISBa]

  //   type PIISSB = Int :: Int :: String :: String :: Boolean :: HNil
  //   type SISBb = the.`ToSum[PIISSB]`.Out
  //   summon[CISBa =:= SISBb]
  // }

  test("SelectAll") {
    import formless.record.{SelectAll => _, *}

    //is there any way to do it without runtime overhead?
    class TypeCaptured[T](val value: T) {
      type _type = T
    }

    def getFieldsByTypesOfSuper[Sub <: HList, Super <: HList](l: Sub)(using sa: SelectAll[Sub, Super]) = sa(l)

    val hsuper = new TypeCaptured("2" :: true :: HNil)
    val hsub = new TypeCaptured(1 :: "2" :: true :: HNil)

    //testing with plain HList
    assertTypedEquals[hsuper._type](hsuper.value, getFieldsByTypesOfSuper[hsub._type, hsuper._type](hsub.value))

    val rsuper = new TypeCaptured(("b" ->> true) :: ("c" ->> "blah") :: HNil)
    val rsub = new TypeCaptured(("a" ->> 1) :: ("b" ->> true) :: ("c" ->> "blah") :: HNil)

    //testing with Record
    assertTypedEquals[rsuper._type](rsuper.value, getFieldsByTypesOfSuper[rsub._type, rsuper._type](rsub.value))
  }

  test("CollectFirst") {
    object Foo extends Poly1{
      given iinst: Case.Aux[Int, Int] = at[Int]{ _ + 1 }
    }
    val hlist1 = "foo" :: 2.0 :: 1 :: HNil
    assertTypedEquals[Int](hlist1.collectFirst(Foo), 2)

    @annotation.unused val hlist2 = "foo" :: 2.0 :: HNil
    illTyped("""hlist2.collectFirst(Foo)""")
  }

  test("Grouper") {
    object toInt extends Poly1 {
      given default[N <: Int]: Case.Aux[N, Int] = at[N](identity)
    }
    def range[R <: HList](a: Int, b: Int)(
      using range: Range.Aux[a.type, b.type, R],
      mapper: Mapper[toInt.type, R]
    ) = mapper(range())

    // group HNil
    assertEquals(HNil: HNil, (HNil: HNil).group(2, 1))
    // group a HList of 4 items into 2 (4/2) tuples of 2 items
    assertTypedEquals[(Int, Int) :: (Int, Int) :: HNil](
      (0, 1) :: (2, 3) :: HNil,
      range(0, 4).group(2, 2)
    )

    // group a HList of 5 items into 2 (5/2) tuples of 2 items
    // the last item does not make a complete partition and is dropped.
    assertEquals(
      (0, 1) :: (2, 3) :: HNil,
      range(0, 5).group(2, 2)
    )

    // uses the step to select the starting point for each partition
    assertEquals(
      (0, 1) :: (4, 5) :: HNil,
      range(0, 6).group(2, 4)
    )

    // if the step is smaller than the partition size, items will be reused
    assertEquals(
      (0, 1) :: (1, 2) :: (2, 3) :: HNil,
      range(0, 4).group(2, 1)
    )
  }

  test("LiftAll") {
    trait F[A]
    implicit object FInt extends F[Int]
    implicit object FString extends F[String]

    assertEquals(HNil, summon[LiftAll[F, HNil]].instances)
    assertEquals(FInt :: HNil, summon[LiftAll[F, Int :: HNil]].instances)
    assertEquals(FString :: FInt :: HNil, summon[LiftAll[F, String :: Int :: HNil]].instances)
    illTyped("summon[LiftAll[F, Long :: String :: Int :: HNil]]")

    assertEquals(FInt :: HNil, LiftAll[F](1 :: HNil).instances)
  }

  test("PadTo") {
    val p1 = (1 :: "a" :: HNil).padTo(3, 0)
    assertTypedEquals[Int :: String :: Int :: HNil](1 :: "a" :: 0 :: HNil, p1)

    val p2 = (1 :: "a" :: HNil).padTo(2, 0)
    assertTypedEquals[Int :: String :: HNil](1 :: "a" :: HNil, p2)

    val p3 = (HNil: HNil).padTo(2, "a")
    assertTypedEquals[String :: String :: HNil]("a" :: "a" :: HNil, p3)

    val p4 = (HNil: HNil).padTo(0, "a")
    assertTypedEquals[HNil](HNil, p4)

    illTyped(""" (1 :: "a" :: HNil).padTo(1, 0) """)
  }

  test("Slice") {
    val r1 = (1 :: "a" :: 3 :: HNil).slice(0, 2)
    assertTypedEquals[Int :: String :: HNil](1 :: "a" :: HNil, r1)

    val r2 = (1 :: "a" :: 3 :: HNil).slice(1, 2)
    assertTypedEquals[String :: HNil]("a" :: HNil, r2)

    val r3 = (1 :: "a" :: 3 :: HNil).slice(2, 3)
    assertTypedEquals[Int :: HNil](3 :: HNil, r3)

    val r4 = (HNil: HNil).slice(0, 0)
    assertTypedEquals[HNil](HNil, r4)

    illTyped(""" (1 :: "a" :: 3 :: HNil).slice(0, 4) """)
    illTyped(""" (1 :: "a" :: 3 :: HNil).slice(1, 0) """)
  }

  test("ModifierAt") {
    // first element
    assertEquals((1, 42 :: 2 :: 3 :: HNil), (1 :: 2 :: 3 :: HNil).updateAtWith(0)(_ => 42))

    //last element
    assertEquals((3, 1 :: 2 :: 42 :: HNil), (1 :: 2 :: 3 :: HNil).updateAtWith(2)(_ => 42))

    //different type
    assertEquals((3, 1 :: 2 :: 42.0 :: HNil), (1 :: 2 :: 3 :: HNil).updateAtWith(2)(_ => 42.0))
  }

  test("Reify") {
    assertTypedEquals(HNil, Reify[HNil].apply())

    type T1 = "a" :: HNil
    assertTypedEquals[T1]("a" :: HNil, Reify[T1].apply())

    type T2 = "a" :: 1 :: "b" :: true :: HNil
    assertTypedEquals[T2](("a" :: 1 :: "b" :: true :: HNil).asInstanceOf[T2], Reify[T2].apply())

    illTyped(""" Reify[String :: Int :: HNil] """)
    illTyped(""" Reify[String :: "a" :: 1 :: "b" :: HNil] """)
  }

  test("Combinations") {
    type I = Int; type S = String

    val r1 = (1 :: "2" :: 3 :: 4 :: HNil).combinations(2)
    assertTypedEquals[
      (I :: S :: HNil) ::
      (I :: I :: HNil) ::
      (I :: I :: HNil) ::
      (S :: I :: HNil) ::
      (S :: I :: HNil) ::
      (I :: I :: HNil) ::
      HNil
    ](
      (1 :: "2" :: HNil) ::
      (1 :: 3 :: HNil) ::
      (1 :: 4 :: HNil) ::
      ("2" :: 3 :: HNil) ::
      ("2" :: 4 :: HNil) ::
      (3 :: 4 :: HNil) ::
      HNil,
      r1,
    )

    val r2 = (1 :: "2" :: 3 :: 4 :: HNil).combinations(3)
    assertTypedEquals[
      (I :: S :: I :: HNil) ::
      (I :: S :: I :: HNil) ::
      (I :: I :: I :: HNil) ::
      (S :: I :: I :: HNil) ::
      HNil
    ](
      (1 :: "2" :: 3 :: HNil) ::
      (1 :: "2" :: 4 :: HNil) ::
      (1 :: 3 :: 4 :: HNil) ::
      ("2" :: 3 :: 4 :: HNil) ::
      HNil,
      r2,
    )

    val r3 = (1 :: "2" :: 3 :: 4 :: HNil).combinations(4)
    assertTypedEquals[(I :: S :: I :: I :: HNil) :: HNil](
      (1 :: "2" :: 3 :: 4 :: HNil) :: HNil, r3)

    val r4 = (1 :: "2" :: 3 :: 4 :: HNil).combinations(5)
    assertTypedEquals[HNil](HNil, r4)

    val r5 = (1 :: "2" :: 3 :: 4 :: HNil).combinations(0)
    assertTypedEquals[HNil :: HNil](HNil :: HNil, r5)
  }

  test("IsHCons") {
    assertTypedEquals[Int :: HNil](23 :: HNil, IsHCons[Int :: HNil].cons(23, HNil))
  }

  test("AuxImplicits") {
    val sr = SplitRight[String :: Int :: Boolean :: HNil, Int]
    summon[sr.Out =:= (String :: Int :: HNil, Boolean :: HNil)]

    val g = Grouper[Int :: String :: Boolean :: HNil, 2, 1]
    summon[g.Out =:= (Int, String) :: (String, Boolean) :: HNil]
  }
}
