# formless

**formless** is a generic programming library for Scala 2 and 3. It takes heavy inspiration from
[shapeless](https://github.com/milessabin/shapeless) in Scala 3 and actually aliases almost everything to shapeless in
Scala 2.

In Scala 3, the `HList` type is redefined, while Scala 2 uses `shapeless.HList` directly. The library defines type
classes and syntax that simplify generic programming.

## Installation

Add the following to your `build.sbt`:

```scala
resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
libraryDependencies += "com.bondlink" %% "formless" % "0.3.0"
```

If you use [`mill`](https://github.com/com-lihaoyi/mill), you can add the following:

```scala
import coursier.maven.MavenRepository

def repositoriesTask = T.task {
  super.repositoriesTask() ++ Seq(MavenRepository("https://raw.githubusercontent.com/mblink/maven-repo/main"))
}

def ivyDeps = Agg(ivy"com.bondlink::formless:0.3.0")
```

## Usage

### `HList`s

Regardless of whether you're using Scala 2 or 3, the `HList` is the core type you'll be working with.

Start with an import:

```scala
import formless.hlist._
```

Then you can build an `HList`:

```scala
val x = 1 :: "foo" :: true :: HNil
// x: ::[Int, ::[String, ::[Boolean, HNil]]] = 1 :: "foo" :: true :: HNil
```

and access the `head` and `tail`:

```scala
x.head
// res0: Int = 1
x.tail
// res1: ::[String, ::[Boolean, HNil]] = "foo" :: true :: HNil
```

If your `HList` is an `HNil` then you'll get a compile-time error when accessing the `head` or `tail`:

```scala
HNil.head
HNil.tail
// error:
// value head is not a member of object formless.hlist.HNil
//   implicit val int: Case.Aux[Int, Int] = at(identity)
//                         ^
// error:
// value tail is not a member of object formless.hlist.HNil - did you mean HNil.wait?
//   implicit val str: Case.Aux[String, Int] = at(_.length)
//                         ^
```

From there, you can perform a number of operations on your `HList`:

```scala
// Append a single element
x :+ "appended"
// res3: ::[Int, ::[String, ::[Boolean, ::[String, HNil]]]] = 1 :: "foo" :: true :: "appended" :: HNil

// Append another `HList`
x ++ (2 :: "bar" :: HNil)
// res4: ::[Int, ::[String, ::[Boolean, ::[Int, ::[String, HNil]]]]] = 1 :: "foo" :: true :: 2 :: "bar" :: HNil

// Prepend another `HList`
2 :: "bar" :: HNil ++ x
// res5: ::[Int, ::[String, ::[Int, ::[String, ::[Boolean, HNil]]]]] = 2 :: "bar" :: 1 :: "foo" :: true :: HNil
// You can also use `:::`
2 :: "bar" :: HNil ::: x
// res6: ::[Int, ::[String, ::[Int, ::[String, ::[Boolean, HNil]]]]] = 2 :: "bar" :: 1 :: "foo" :: true :: HNil

// Get an element by index using `apply`
x(0)
// res7: Int = 1
x(1)
// res8: String = "foo"

// Get an element by index using `at`, either by passing an argument or an Int-literal type parameter
x.at(0)
// res9: Int = 1
x.at[1]
// res10: String = "foo"

// Get the last element
x.last
// res11: Boolean = true

// Get everything but the last element, aka `init`
x.init
// res12: ::[Int, ::[String, HNil]] = 1 :: "foo" :: HNil

// Get the first element of a given type
x.select[Boolean]
// res13: Boolean = true

// Get all elements at given indices
x.selectManyType[0 :: 2 :: HNil]
// res14: ::[Int, ::[Boolean, HNil]] = 1 :: true :: HNil

// Get a range of elements, inclusive on the low end, exclusive on the high end
x.selectRange[0, 2]
// res15: ::[Int, ::[String, HNil]] = 1 :: "foo" :: HNil
x.selectRange(0, 2)
// res16: ::[Int, ::[String, HNil]] = 1 :: "foo" :: HNil

// Filter to only include elements of a given type
x.filter[Boolean]
// res17: ::[Boolean, HNil] = true :: HNil

// Filter to exclude elements of a given type
x.filterNot[Boolean]
// res18: ::[Int, ::[String, HNil]] = 1 :: "foo" :: HNil

// Partition into two `HList`s -- one of elements of the given type, one of the remaining elements
x.partition[Boolean]
// res19: Tuple2[Prefix, Suffix] = (true :: HNil, 1 :: "foo" :: HNil)

// Remove the first element of a given type
x.removeElem[Boolean]
// res20: Tuple2[Boolean, ::[Int, ::[String, HNil]]] = (
//   true,
//   1 :: "foo" :: HNil
// )

// Remove the first elements of a given set of types
x.removeAll[String :: Boolean :: HNil]
// res21: Tuple2[::[String, ::[Boolean, HNil]], ::[Int, HNil]] = (
//   "foo" :: true :: HNil,
//   1 :: HNil
// )

// Find the union with another `HList`
x.union(3 :: () :: Some("baz") :: HNil)
// res22: ::[Int, ::[String, ::[Boolean, ::[Unit, ::[Some[String], HNil]]]]] = 1 :: "foo" :: true :: () :: Some(value = "baz") :: HNil

// Find the intersection with another `HList`
x.intersect[Int :: Boolean :: HNil]
// res23: ::[Int, ::[Boolean, HNil]] = 1 :: true :: HNil

// Find the difference with another `HList`
x.diff[String :: Boolean :: HNil]
// res24: ::[Int, HNil] = 1 :: HNil

// Replace the first element of a given type with another value of the same type
x.replace("baz")
// res25: Tuple2[String, ::[Int, ::[String, ::[Boolean, HNil]]]] = (
//   "foo",
//   1 :: "baz" :: true :: HNil
// )

// Update the first element of a given type with a function
x.updateTypeWith((_: String).length)
// res26: ::[Int, ::[Int, ::[Boolean, HNil]]] = 1 :: 3 :: true :: HNil

// Update an element at a given index with a function
x.updateAtWith(1)(_.length)
// res27: Tuple2[String, ::[Int, ::[Int, ::[Boolean, HNil]]]] = (
//   "foo",
//   1 :: 3 :: true :: HNil
// )

// Take the first N elements
x.take(2)
// res28: ::[Int, ::[String, HNil]] = 1 :: "foo" :: HNil

// Drop the first N elements
x.drop(2)
// res29: ::[Boolean, HNil] = true :: HNil

// Split the `HList` at a given index
x.split(1)
// res30: Tuple2[::[Int, HNil], ::[String, ::[Boolean, HNil]]] = (
//   1 :: HNil,
//   "foo" :: true :: HNil
// )

// Split the `HList` at the first element of a given type
x.splitLeft[String]
// res31: Tuple2[::[Int, HNil], ::[String, ::[Boolean, HNil]]] = (
//   1 :: HNil,
//   "foo" :: true :: HNil
// )

// Split the `HList` at the last element of a given type
x.splitRight[String]
// res32: Tuple2[::[Int, ::[String, HNil]], ::[Boolean, HNil]] = (
//   1 :: "foo" :: HNil,
//   true :: HNil
// )

// Reorder the `HList` to match the order of elements in another `HList`
x.align[Boolean :: Int :: String :: HNil]
// res33: ::[Boolean, ::[Int, ::[String, HNil]]] = true :: 1 :: "foo" :: HNil

// Reverse the `HList`
x.reverse_
// res34: ::[Boolean, ::[String, ::[Int, HNil]]] = true :: "foo" :: 1 :: HNil

// Map a polymorphic function (a `Poly`) over the `HList`
object mapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, String] = at(b => if (b) "yes" else "no")
}

x.map(mapFn)
// res35: ::[Boolean, ::[Int, ::[String, HNil]]] = false :: 3 :: "yes" :: HNil

// FlatMap a polymorphic function over the `HList`
object flatMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int :: Int :: HNil] = at(i => i :: (i * 2) :: HNil)
  implicit val str: Case.Aux[String, String :: String :: HNil] = at(s => s :: s.reverse :: HNil)
  implicit val bool: Case.Aux[Boolean, Boolean :: Boolean :: HNil] = at(b => b :: !b :: HNil)
}

x.flatMap(flatMapFn)
// res36: ::[Int, ::[Int, ::[String, ::[String, ::[Boolean, ::[Boolean, HNil]]]]]] = 1 :: 2 :: "foo" :: "oof" :: true :: false :: HNil

// TODO - Prepend an element to every `HList` in this matrix
// x.mapCons()

// Replace every element in the `HList` with a constant value
x.mapConst(42)
// res37: ::[Int, ::[Int, ::[Int, HNil]]] = 42 :: 42 :: 42 :: HNil

// Collect the elements of the `HList` using a polymorphic function
object collectFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
}

x.collect(collectFn)
// res38: ::[Boolean, ::[Int, HNil]] = false :: 3 :: HNil

object foldMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int] = at(identity)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, Int] = at(b => if (b) 1 else 0)
}

// Map a polymorphic function over the `HList` and fold the result using a function
x.foldMap(0)(foldMapFn)(_ + _)
// res39: Int = 5

// Fold over the `HList` with a polymorphic function starting from the beginning
object foldLeftFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.foldLeft(0)(foldLeftFn)
// res40: Int = 5

// Fold over the `HList` with a polymorphic function starting from the end
object foldRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[String, Int, Int] = at((s, acc) => acc + s.length)
  implicit val bool: Case.Aux[Boolean, Int, Int] = at((b, acc) => acc + (if (b) 1 else 0))
}

x.foldRight(0)(foldRightFn)
// res41: Int = 5

// Reduce the `HList` with a polymorphic function using the first element as the initial value
object reduceLeftFn extends Poly2 {
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.reduceLeft(reduceLeftFn)
// res42: Int = 5

// Reduce the `HList` with a polymorphic function using the last element as the initial value
object reduceRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Boolean, Boolean] = at((i, acc) => (i % 2 == 0) || acc)
  implicit val str: Case.Aux[String, Boolean, Boolean] = at((s, acc) => s.nonEmpty && acc)
}

x.reduceRight(reduceRightFn)
// res43: Boolean = true

// Repeat the `HList` a given number of times
x.repeat[3]
// res44: ::[Int, ::[String, ::[Boolean, ::[Int, ::[String, ::[Boolean, ::[Int, ::[String, ::[Boolean, HNil]]]]]]]]] = 1 :: "foo" :: true :: 1 :: "foo" :: true :: 1 :: "foo" :: true :: HNil

// Transform of a `HList` of functions and corresponding elements by passing the element to the function
val fns =
  ((i: Int) => i % 2 == 0) ::
  ((s: String) => s.length) ::
  ((b: Boolean) => if (b) "yes" else "no") ::
  HNil
// fns: ::[Function1[Int, Boolean], ::[Function1[String, Int], ::[Function1[Boolean, String], HNil]]] = repl.MdocSession$MdocApp$$Lambda/0x000000030272a028@c3bec22 :: repl.MdocSession$MdocApp$$Lambda/0x0000000302729c60@3c72f62f :: repl.MdocSession$MdocApp$$Lambda/0x0000000302729898@2991ea67 :: HNil

fns.zipApply(x)
// res45: ::[Boolean, ::[Int, ::[String, HNil]]] = false :: 3 :: "yes" :: HNil
```
