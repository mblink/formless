package formless
package hlist

import munit.Location
import formless.test._
import formless.testutil._

trait HListTestsCompat { self: HListTests =>
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

    def getUnifier[L <: HList, Out <: HList](@annotation.nowarn("msg=unused") l: L)(using u: Unifier.Aux[L, Out]) = u

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

    val _ = ToArray[HNil, Unit]
    val _ = ToArray[HNil, Int]

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
}
