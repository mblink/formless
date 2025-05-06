# formless

**formless** is a generic programming library for Scala 2 and 3. It takes heavy inspiration from
[shapeless](https://github.com/milessabin/shapeless) in Scala 3 and actually aliases almost everything to shapeless in
Scala 2.

In Scala 3, the `HList` type is redefined, while Scala 2 uses `shapeless.HList` directly. The library defines type
classes and syntax that simplify generic programming.

## Installation

Add the following to your `build.sbt`:

```scala
resolvers += "bondlink-maven-repo" at "@BL_MAVEN_REPO_URL@"
libraryDependencies += "com.bondlink" %% "formless" % "@VERSION@"
```

If you use [`mill`](https://github.com/com-lihaoyi/mill), you can add the following:

```scala
import coursier.maven.MavenRepository

def repositoriesTask = T.task {
  super.repositoriesTask() ++ Seq(MavenRepository("@BL_MAVEN_REPO_URL@"))
}

def ivyDeps = Agg(ivy"com.bondlink::formless:@VERSION@")
```

## Usage

### `HList`s

Regardless of whether you're using Scala 2 or 3, the `HList` is the core type you'll be working with.

Start with an import:

```scala mdoc
import formless.hlist._
```

Then you can build an `HList`:

```scala mdoc
val x = 1 :: "foo" :: true :: HNil
```

and access the `head` and `tail`:

```scala mdoc
x.head
x.tail
```

If your `HList` is an `HNil` then you'll get a compile-time error when accessing the `head` or `tail`:

```scala mdoc:fail
HNil.head
HNil.tail
```

From there, you can perform a number of operations on your `HList`:

```scala mdoc
// Append a single element
x :+ "appended"

// Append another `HList`
x ++ (2 :: "bar" :: HNil)

// Prepend another `HList`
2 :: "bar" :: HNil ++ x
// You can also use `:::`
2 :: "bar" :: HNil ::: x

// Get an element by index using `apply`
x(0)
x(1)

// Get an element by index using `at`, either by passing an argument or an Int-literal type parameter
x.at(0)
x.at[@SHAPELESS_NAT_PREFIX@1]

// Get the last element
x.last

// Get everything but the last element, aka `init`
x.init

// Get the first element of a given type
x.select[Boolean]

// Get all elements at given indices
x.selectManyType[@SHAPELESS_NAT_PREFIX@0 :: @SHAPELESS_NAT_PREFIX@2 :: HNil]

// Get a range of elements, inclusive on the low end, exclusive on the high end
x.selectRange[@SHAPELESS_NAT_PREFIX@0, @SHAPELESS_NAT_PREFIX@2]
x.selectRange(0, 2)

// Filter to only include elements of a given type
x.filter[Boolean]

// Filter to exclude elements of a given type
x.filterNot[Boolean]

// Partition into two `HList`s -- one of elements of the given type, one of the remaining elements
x.partition[Boolean]

// Remove the first element of a given type
x.removeElem[Boolean]

// Remove the first elements of a given set of types
x.removeAll[String :: Boolean :: HNil]

// Find the union with another `HList`
x.union(3 :: () :: Some("baz") :: HNil)

// Find the intersection with another `HList`
x.intersect[Int :: Boolean :: HNil]

// Find the difference with another `HList`
x.diff[String :: Boolean :: HNil]

// Replace the first element of a given type with another value of the same type
x.replace("baz")

// Update the first element of a given type with a function
x.updateWith((_: String).length)

// Update an element at a given index with a function
x.updateAtWith(1)(_.length)

// Take the first N elements
x.take(2)

// Drop the first N elements
x.drop(2)

// Split the `HList` at a given index
x.split(1)

// Split the `HList` at the first element of a given type
x.splitLeft[String]

// Split the `HList` at the last element of a given type
x.splitRight[String]

// Reorder the `HList` to match the order of elements in another `HList`
x.align[Boolean :: Int :: String :: HNil]

// Reverse the `HList`
x.reverse

// Map a polymorphic function (a `Poly`) over the `HList`
object mapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, String] = at(b => if (b) "yes" else "no")
}

x.map(mapFn)

// FlatMap a polymorphic function over the `HList`
object flatMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int :: Int :: HNil] = at(i => i :: (i * 2) :: HNil)
  implicit val str: Case.Aux[String, String :: String :: HNil] = at(s => s :: s.reverse :: HNil)
  implicit val bool: Case.Aux[Boolean, Boolean :: Boolean :: HNil] = at(b => b :: !b :: HNil)
}

x.flatMap(flatMapFn)

// TODO - Prepend an element to every `HList` in this matrix
// x.mapCons()

// Replace every element in the `HList` with a constant value
x.mapConst(42)

// Collect the elements of the `HList` using a polymorphic function
object collectFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
}

x.collect(collectFn)

object foldMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int] = at(identity)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, Int] = at(b => if (b) 1 else 0)
}

// Map a polymorphic function over the `HList` and fold the result using a function
x.foldMap(0)(foldMapFn)(_ + _)

// Fold over the `HList` with a polymorphic function starting from the beginning
object foldLeftFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.foldLeft(0)(foldLeftFn)

// Fold over the `HList` with a polymorphic function starting from the end
object foldRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[String, Int, Int] = at((s, acc) => acc + s.length)
  implicit val bool: Case.Aux[Boolean, Int, Int] = at((b, acc) => acc + (if (b) 1 else 0))
}

x.foldRight(0)(foldRightFn)

// Reduce the `HList` with a polymorphic function using the first element as the initial value
object reduceLeftFn extends Poly2 {
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.reduceLeft(reduceLeftFn)

// Reduce the `HList` with a polymorphic function using the last element as the initial value
object reduceRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Boolean, Boolean] = at((i, acc) => (i % 2 == 0) || acc)
  implicit val str: Case.Aux[String, Boolean, Boolean] = at((s, acc) => s.nonEmpty && acc)
}

x.reduceRight(reduceRightFn)

// Repeat the `HList` a given number of times
x.repeat[@SHAPELESS_NAT_PREFIX@3]

// Transform of a `HList` of functions and corresponding elements by passing the element to the function
val fns =
  ((i: Int) => i % 2 == 0) ::
  ((s: String) => s.length) ::
  ((b: Boolean) => if (b) "yes" else "no") ::
  HNil

fns.zipApply(x)
```
