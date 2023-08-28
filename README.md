# formless

**formless** is a generic programming library for Scala 2 and 3. It takes heavy inspiration from
[shapeless](https://github.com/milessabin/shapeless) in Scala 3 and actually aliases almost everything to shapeless in
Scala 2.

In Scala 3, all code uses native `Tuple`s, while Scala 2 uses `shapeless.HList`s. The library defines type classes and
syntax that simplify generic programming.

## Installation

Add the following to your `build.sbt`:

```scala
resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
libraryDependencies += "com.bondlink" %%"formless" % "0.1.0-SNAPSHOT"
```

If you use [`mill`](https://github.com/com-lihaoyi/mill), you can add the following:

```scala
import coursier.maven.MavenRepository

def repositoriesTask = T.task {
  super.repositoriesTask() ++ Seq(MavenRepository("https://raw.githubusercontent.com/mblink/maven-repo/main"))
}

def ivyDeps = Agg(ivy"com.bondlink::formless:0.1.0-SNAPSHOT")
```

## Usage

### `Tuple`s

Regardless of whether you're using Scala 2 or 3, the `Tuple` is the core type you'll be working with.

Start with an import:

```scala
import formless.tuple._
```

Then you can build a `Tuple`:

```scala
val x = 1 *: "foo" *: true *: EmptyTuple
// x: *:[Int, *:[String, *:[Boolean, EmptyTuple]]] = (1, "foo", true)
```

and access the `head` and `tail`:

```scala
x.head
// res0: Int = 1
x.tail
// res1: *:[String, *:[Boolean, EmptyTuple]] = ("foo", true)
```

If your `Tuple` is an `EmptyTuple` then you'll get a compile-time error when accessing the `head` or `tail`:

```scala
EmptyTuple.head
EmptyTuple.tail
// error:
// value head is not a member of formless.tuple.EmptyTuple
//   implicit val int: Case.Aux[Int, Int] = at(identity)
//                         ^
// error:
// value tail is not a member of formless.tuple.EmptyTuple - did you mean EmptyTuple.take?
//   implicit val str: Case.Aux[String, Int] = at(_.length)
//                         ^
```

From there, you can perform a number of operations on your `Tuple`:

```scala
// Append a single element
x :+ "appended"
// res3: *:[Int, *:[String, *:[Boolean, *:[String, EmptyTuple]]]] = (
//   1,
//   "foo",
//   true,
//   "appended"
// )

// Append another `Tuple`
x ++ 2 *: "bar" *: EmptyTuple
// res4: *:[Int, *:[String, *:[Boolean, *:[Int, *:[String, EmptyTuple]]]]] = (
//   1,
//   "foo",
//   true,
//   2,
//   "bar"
// )

// Prepend another `Tuple`
2 *: "bar" *: EmptyTuple ++ x
// res5: *:[Int, *:[String, x]] = (2, "bar", 1, "foo", true)
// You can also use `:::`
2 *: "bar" *: EmptyTuple ::: x
// res6: *:[Int, *:[String, Tuple3[Int, String, Boolean]]] = (
//   2,
//   "bar",
//   1,
//   "foo",
//   true
// )

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
// res12: *:[Int, *:[String, EmptyTuple]] = (1, "foo")

// Get the first element of a given type
x.select[Boolean]
// res13: Boolean = true

// Get all elements at given indices
x.selectManyType[0 *: 2 *: EmptyTuple]
// res14: *:[Int, *:[Boolean, EmptyTuple]] = (1, true)

// Get a range of elements, inclusive on the low end, exclusive on the high end
x.selectRange[0, 2]
// res15: *:[Int, *:[String, EmptyTuple]] = (1, "foo")
x.selectRange(0, 2)
// res16: *:[Int, *:[String, EmptyTuple]] = (1, "foo")

// Filter to only include elements of a given type
x.filter[Boolean]
// res17: *:[Boolean, EmptyTuple] = Tuple1(_1 = true)

// Filter to exclude elements of a given type
x.filterNot[Boolean]
// res18: *:[Int, *:[String, EmptyTuple]] = (1, "foo")

// Partition into two tuples -- one of elements of the given type, one of the remaining elements
x.partition[Boolean]
// res19: Tuple2[Prefix, Suffix] = (Tuple1(_1 = true), (1, "foo"))

// Remove the first element of a given type
x.removeElem[Boolean]
// res20: Tuple2[Boolean, *:[Int, *:[String, EmptyTuple]]] = (true, (1, "foo"))

// Remove the first elements of a given set of types
x.removeAll[String *: Boolean *: EmptyTuple]
// res21: Tuple2[*:[String, *:[Boolean, EmptyTuple]], *:[Int, EmptyTuple]] = (
//   ("foo", true),
//   Tuple1(_1 = 1)
// )

// Find the union with another `Tuple`
x.union(3 *: () *: Some("baz") *: EmptyTuple)
// res22: *:[Int, *:[String, *:[Boolean, *:[Unit, *:[Some[String], EmptyTuple]]]]] = (
//   1,
//   "foo",
//   true,
//   (),
//   Some(value = "baz")
// )

// Find the intersection with another `Tuple`
x.intersect[Int *: Boolean *: EmptyTuple]
// res23: *:[Int, *:[Boolean, EmptyTuple]] = (1, true)

// Find the difference with another `Tuple`
x.diff[String *: Boolean *: EmptyTuple]
// res24: *:[Int, EmptyTuple] = Tuple1(_1 = 1)

// Replace the first element of a given type with another value of the same type
x.replace("baz")
// res25: Tuple2[String, *:[Int, *:[String, *:[Boolean, EmptyTuple]]]] = (
//   "foo",
//   (1, "baz", true)
// )

// Update the first element of a given type with a function
x.updateTypeWith((_: String).length)
// res26: *:[Int, *:[Int, *:[Boolean, EmptyTuple]]] = (1, 3, true)

// Update an element at a given index with a function
x.updateAtWith(1)(_.length)
// res27: Tuple2[String, *:[Int, *:[Int, *:[Boolean, EmptyTuple]]]] = (
//   "foo",
//   (1, 3, true)
// )

// Take the first N elements
x.take(2)
// res28: *:[Int, *:[String, EmptyTuple]] = (1, "foo")

// Drop the first N elements
x.drop(2)
// res29: *:[Boolean, EmptyTuple] = Tuple1(_1 = true)

// Split the `Tuple` at a given index
x.split(1)
// res30: Tuple2[*:[Int, EmptyTuple], *:[String, *:[Boolean, EmptyTuple]]] = (
//   Tuple1(_1 = 1),
//   ("foo", true)
// )

// Split the `Tuple` at the first element of a given type
x.splitLeft[String]
// res31: Tuple2[*:[Int, EmptyTuple], *:[String, *:[Boolean, EmptyTuple]]] = (
//   Tuple1(_1 = 1),
//   ("foo", true)
// )

// Split the `Tuple` at the last element of a given type
x.splitRight[String]
// res32: Tuple2[*:[Int, *:[String, EmptyTuple]], *:[Boolean, EmptyTuple]] = (
//   (1, "foo"),
//   Tuple1(_1 = true)
// )

// Reorder the `Tuple` to match the order of elements in another `Tuple`
x.align[Boolean *: Int *: String *: EmptyTuple]
// res33: *:[Boolean, *:[Int, *:[String, EmptyTuple]]] = (true, 1, "foo")

// Reverse the `Tuple`
x.reverse
// res34: *:[Boolean, *:[String, *:[Int, EmptyTuple]]] = (true, "foo", 1)

// Map a polymorphic function (a `Poly`) over the `Tuple`
object mapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, String] = at(b => if (b) "yes" else "no")
}

x.mapPoly(mapFn)
// res35: *:[Boolean, *:[Int, *:[String, EmptyTuple]]] = (false, 3, "yes")

// FlatMap a polymorphic function over the `Tuple`
object flatMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int *: Int *: EmptyTuple] = at(i => i *: (i * 2) *: EmptyTuple)
  implicit val str: Case.Aux[String, String *: String *: EmptyTuple] = at(s => s *: s.reverse *: EmptyTuple)
  implicit val bool: Case.Aux[Boolean, Boolean *: Boolean *: EmptyTuple] = at(b => b *: !b *: EmptyTuple)
}

x.flatMap(flatMapFn)
// res36: *:[Int, *:[Int, *:[String, *:[String, *:[Boolean, *:[Boolean, EmptyTuple]]]]]] = (
//   1,
//   2,
//   "foo",
//   "oof",
//   true,
//   false
// )

// TODO - Prepend an element to every `Tuple` in this matrix
// x.mapCons()

// Replace every element in the `Tuple` with a constant value
x.mapConst(42)
// res37: *:[Int, *:[Int, *:[Int, EmptyTuple]]] = (42, 42, 42)

// Collect the elements of the `Tuple` using a polymorphic function
object collectFn extends Poly1 {
  implicit val int: Case.Aux[Int, Boolean] = at(_ % 2 == 0)
  implicit val str: Case.Aux[String, Int] = at(_.length)
}

x.collect(collectFn)
// res38: *:[Boolean, *:[Int, EmptyTuple]] = (false, 3)

object foldMapFn extends Poly1 {
  implicit val int: Case.Aux[Int, Int] = at(identity)
  implicit val str: Case.Aux[String, Int] = at(_.length)
  implicit val bool: Case.Aux[Boolean, Int] = at(b => if (b) 1 else 0)
}

// Map a polymorphic function over the `Tuple` and fold the result using a function
x.foldMap(0)(foldMapFn)(_ + _)
// res39: Int = 5

// Fold over the `Tuple` with a polymorphic function starting from the beginning
object foldLeftFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.foldLeft(0)(foldLeftFn)
// res40: Int = 5

// Fold over the `Tuple` with a polymorphic function starting from the end
object foldRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Int, Int] = at(_ + _)
  implicit val str: Case.Aux[String, Int, Int] = at((s, acc) => acc + s.length)
  implicit val bool: Case.Aux[Boolean, Int, Int] = at((b, acc) => acc + (if (b) 1 else 0))
}

x.foldRight(0)(foldRightFn)
// res41: Int = 5

// Reduce the `Tuple` with a polymorphic function using the first element as the initial value
object reduceLeftFn extends Poly2 {
  implicit val str: Case.Aux[Int, String, Int] = at((acc, s) => acc + s.length)
  implicit val bool: Case.Aux[Int, Boolean, Int] = at((acc, b) => acc + (if (b) 1 else 0))
}

x.reduceLeft(reduceLeftFn)
// res42: Int = 5

// Reduce the `Tuple` with a polymorphic function using the last element as the initial value
object reduceRightFn extends Poly2 {
  implicit val int: Case.Aux[Int, Boolean, Boolean] = at((i, acc) => (i % 2 == 0) || acc)
  implicit val str: Case.Aux[String, Boolean, Boolean] = at((s, acc) => s.nonEmpty && acc)
}

x.reduceRight(reduceRightFn)
// res43: Boolean = true

// Repeat the `Tuple` a given number of times
x.repeat[3]
// res44: *:[Int, *:[String, *:[Boolean, *:[Int, *:[String, *:[Boolean, Tuple3[Int, String, Boolean]]]]]]] = (
//   1,
//   "foo",
//   true,
//   1,
//   "foo",
//   true,
//   1,
//   "foo",
//   true
// )

// Transform of a `Tuple` of functions and corresponding elements by passing the element to the function
val fns =
  ((i: Int) => i % 2 == 0) *:
  ((s: String) => s.length) *:
  ((b: Boolean) => if (b) "yes" else "no") *:
  EmptyTuple
// fns: *:[Function1[Int, Boolean], *:[Function1[String, Int], *:[Function1[Boolean, String], EmptyTuple]]] = (
//   repl.MdocSession$MdocApp$$Lambda$13940/0x00000070029a64d0@6b7908ba,
//   repl.MdocSession$MdocApp$$Lambda$13939/0x00000070029a6100@565c9c02,
//   repl.MdocSession$MdocApp$$Lambda$13938/0x00000070029a5d30@5a5deaef
// )

fns.zipApply(x)
// res45: *:[Boolean, *:[Int, *:[String, EmptyTuple]]] = (false, 3, "yes")
```
