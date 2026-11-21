# formless: HLists, extensible records and generic programming for Scala 3 and Scala 2.13

[![CI](https://github.com/mblink/formless/actions/workflows/ci.yml/badge.svg)](https://github.com/mblink/formless/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.bondlink/formless_3.svg)](https://central.sonatype.com/artifact/com.bondlink/formless_3)
[![Scaladoc](https://javadoc.io/badge2/com.bondlink/formless_3/scaladoc.svg)](https://javadoc.io/doc/com.bondlink/formless_3)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
![Scala 3](https://img.shields.io/badge/Scala-3.3%20LTS%20%7C%203.8-red.svg)
![Scala 2.13](https://img.shields.io/badge/Scala-2.13-red.svg)
![Platforms](https://img.shields.io/badge/platforms-JVM%20%7C%20Scala.js%20%7C%20Scala%20Native-informational.svg)

**formless** is a type-level generic programming library for Scala: heterogeneous lists (`HList`), extensible records with singleton-typed keys, `Generic` and `LabelledGeneric` conversions between case classes and records, polymorphic functions (`Poly`) and tagged types. It is a [shapeless](https://github.com/milessabin/shapeless) 2 style API that works the same on Scala 2.13 and Scala 3, so code written against shapeless 2 can move to Scala 3 without being rewritten.

On Scala 3 formless is implemented natively, with match types, `scala.compiletime` and a few quoted macros, and has no dependency on shapeless. On Scala 2.13 it is a thin layer over shapeless 2. The public API is the same on both, and every module is built for the JVM, Scala.js and Scala Native.

## Why formless

- **One API for Scala 2.13 and Scala 3.** Cross-build generic code without `scala-2` and `scala-3` source splits, or migrate a shapeless 2 codebase to Scala 3 one module at a time.
- **Fast to compile.** Record field and `HList` element lookups on Scala 3 are a single macro walk that builds each result type directly, instead of chains of implicit searches and match types: 3 to 20 times faster to compile than 0.8, allocating 3 to 35 times less memory. See [Compile-time performance](#compile-time-performance).
- **Sound lookups.** A field is found only when the record's type proves it is there, and proven absent only when no field could possibly have the key. Abstract tails, abstract fields, unions and intersections are handled rather than guessed at: a lookup that can't be decided until a type parameter is known, or that would pick different elements in different arms of a union, is a compile error rather than a guess, and `LacksKey` never lies.
- **Never crashes the compiler.** Every lookup macro turns an unexpected input into an ordinary compile error, and long or self-referential types are refused instead of looping.
- **Production tested.** formless is used across the BondLink codebase, a large Scala 3 application with heavy use of records.

## Compile-time performance

formless 0.9 rebuilt how records and `HList`s find a field or an element by type. Compiling code that uses those lookups is several times faster and allocates a fraction of the memory it did in 0.8:

| Workload | Wall time, 0.8 → 0.9 | CPU time, 0.8 → 0.9 | Memory allocated, 0.8 → 0.9 |
|---|---|---|---|
| 320 record lookups (`Selector`, `Updater`, `Modifier`, `Remover`) on an 80-field record with string keys | 15.5 s → 1.8 s (**8.6×**) | 18.6 s → 2.9 s (**6.4×**) | 4.95 GB → 0.60 GB (**8.2×**) |
| The same with integer keys | 15.3 s → 1.7 s (**8.8×**) | 18.1 s → 2.6 s (**7.0×**) | 4.95 GB → 0.60 GB (**8.2×**) |
| 160 appends and absence proofs (`Updater`, `LacksKey`) on an 80-field record | 3.3 s → 0.9 s (**3.6×**) | 4.4 s → 1.4 s (**3.0×**) | 1.13 GB → 0.39 GB (**2.9×**) |
| 120 `HList` element lookups (`Remove`, `Replacer`, `SplitLeft`, `SplitRight`) on a 30-element list | 2.2 s → 0.5 s (**4.8×**) | 3.2 s → 1.1 s (**3.0×**) | 1.28 GB → 0.14 GB (**9.1×**) |
| The same on a 60-element list | 20.3 s → 1.0 s (**20×**) | 24.4 s → 1.6 s (**15×**) | 10.9 GB → 0.31 GB (**35×**) |

In 0.8, doubling the `HList` from 30 to 60 elements made its lookups about eight times as costly; in 0.9 they take about 1.5 times as much CPU time and allocate about 2.2 times as much memory, close to the linear growth of twice as many lookups each walking a list twice as long, so the gap widens as lists get longer. CI checks that this ratio stays below 5 for allocation. Peak memory of the compiler process fell from about 1.25 GB to 1.04 GB.

Each figure is the median of six runs compiling one generated file on a warm sbt server (Scala 3.3.8, JDK 25, Apple Silicon), with 0.8 and 0.9 runs interleaved so that load from other work on the machine affects both alike. Allocation was read from the JVM over JMX and varied by under 1% between runs.

## Installation

formless is published to Maven Central for Scala 2.13 and Scala 3, on the JVM, Scala.js and Scala Native. With sbt:

```scala
libraryDependencies += "com.bondlink" %% "formless" % "@VERSION@"
// or, for Scala.js and Scala Native
libraryDependencies += "com.bondlink" %%% "formless" % "@VERSION@"
```

With [Mill](https://mill-build.org):

```scala
def ivyDeps = Agg(ivy"com.bondlink::formless:@VERSION@")
```

With [scala-cli](https://scala-cli.virtuslab.org):

```scala
//> using dep com.bondlink::formless:@VERSION@
```

API documentation is on [javadoc.io](https://javadoc.io/doc/com.bondlink/formless_3).

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

### Records

A record is an `HList` of fields, each a value tagged with a singleton-typed key. Import `formless.record._` to build and use them.

`formless.record` also has type classes named like `HList` ones (`Selector`, `Modifier`, `Remove`, `RemoveAll` and `SelectAll`), so importing both `formless.hlist._` and `formless.record._` makes those names ambiguous. To use both, import `formless.record.syntax._` instead of `formless.record._`: it has everything for building and using records except the type classes, which you can still name as `formless.record.Selector` and so on. Use one of the two record imports in a file, not both.

The record syntax adds `apply`, `updateWith` and `replace` to every `HList`, taking a key, so they hide the `HList` methods of the same names in files that import it. Using one of them on a list with no fields is a compile error saying so; call the `HList` method as `formless.hlist.HList.toHListOps(list).updateWith(...)`.

```scala mdoc:reset
import formless.hlist.{::, HNil}
import formless.record._

val book = ("title" ->> "Dune") :: ("year" ->> 1965) :: ("inPrint" ->> true) :: HNil
```

Fields are looked up by key at compile time:

```scala mdoc
// Get a field's value
book.get("title")
book("year")

// Replace a field's value with one of the same type
book.replace("year", 1966)

// Update a field with a function, possibly changing its type
book.updateWith("year")(_.toString)

// Replace a field with a value of the same type, or add it at the end if the record has no field with that key.
// As in shapeless, a value of another type is added as a new field even when the key is there; use updateWith to
// change a field's type.
book.updated("year", 1966)
book.updated("author", "Frank Herbert")
book + ("pages" ->> 412)

// Remove a field
book - "inPrint"
book.remove("inPrint")

// Rename a field
book.renameField("inPrint", "available")

// Merge with another record: its fields replace those with the same key and value type, and the rest are added at
// the end
book.merge(("year" ->> 1966) :: ("author" ->> "Frank Herbert") :: HNil)

// The keys, the values, and the fields as key-value pairs
book.keys
book.values
book.fields
```

Looking up a key the record doesn't have is a compile-time error:

```scala mdoc:fail
book.get("author")
```

A lookup succeeds only when the answer is certain from the record's type. A field that could be inside an abstract part of the type, such as a type parameter tail `T` in `("a" ->> Int) :: T`, is neither found nor proven absent: `get("b")` on that record doesn't compile, and neither does `updated("b", ...)`, since the tail might already have a `"b"`. Fields before the abstract part work as usual. The same applies to the `HList` operations that find an element by type, such as `removeElem`, `replaceType`, `updateWith`, `splitLeft` and `splitRight`.

### Case classes as `HList`s and records

`Generic` converts a case class to an `HList` of its fields and back. `LabelledGeneric` does the same with a record, keyed by the field names, so you can work on any case class with the record operations above:

```scala mdoc:reset:silent
import formless.hlist.Generic
import formless.record._

final case class Book(title: String, year: Int, inPrint: Boolean)

val dune = Book("Dune", 1965, true)
val gen = Generic[Book]
val labelled = LabelledGeneric[Book]
```

```scala mdoc
gen.to(dune)
gen.from("Emma" :: 1815 :: false :: formless.hlist.HNil)

val rec = labelled.to(dune)
rec.get("year")
labelled.from(rec.updated("year", 1966))
```

### Tagged types

Tag a type to tell values apart without a runtime wrapper. `@@` is a tagged type, and `tag` makes one:

```scala mdoc:reset
import formless.tagged._

trait UserId
val id: Int @@ UserId = tag[UserId](42)
id.untag
```

## Upgrading from 0.8

0.9 is source compatible with 0.8 for code that uses records and `HList`s through their syntax and type classes, but not binary compatible, so libraries built against 0.8 need rebuilding. On Scala 3:

- `formless.hlist.FindField` and `formless.record.FindField`, the building blocks of 0.8's lookups, are gone; use the type class for the operation (`Selector`, `Updater`, `Remove` and so on) instead.
- The record syntax's `apply`, `replace` and `updateWith` take a leading `using` clause that checks the list has record fields, so a call that passes their evidence explicitly gains an argument: `record("a")(using selector)` becomes `record("a")(using summon)(using selector)`. Calls that leave the evidence to be found are unchanged.
- A lookup that 0.8 resolved but that can't be decided soundly, such as a field at different indexes in the arms of a union of records, or one hidden behind an abstract tail, is now a compile error naming the reason.
- formless needs Java 17 or newer.

## Migrating from shapeless 2

formless follows shapeless 2's names and syntax, so most code needs only its imports changed:

| shapeless 2 | formless |
|---|---|
| `import shapeless._` | `import formless.hlist._` |
| `import shapeless.record._`, `import shapeless.syntax.singleton._` | `import formless.record._` |
| `import shapeless.tag._`, `@@` | `import formless.tagged._`, `@@` |
| `shapeless.ops.hlist.*`, `shapeless.ops.record.*` | the type class of the same name in `formless.hlist` or `formless.record` |
| `Nat` indices such as `_2` | `Int` literal types such as `2` on Scala 3; `shapeless.nat._2` on Scala 2.13 |

On Scala 2.13 formless's `HList`, `::` and `HNil` are shapeless's own, so the two mix freely while you migrate.

## For AI coding agents

A summary for tools that write code against formless:

- Imports: `formless.hlist._` for `HList`, `::`, `HNil`, `Poly` and the `HList` type classes; `formless.record._` for `->>`, record syntax and the record type classes. Five type class names (`Selector`, `Modifier`, `Remove`, `RemoveAll`, `SelectAll`) exist in both, so alongside `formless.hlist._` import `formless.record.syntax._` (record building and syntax, no type classes) instead of `formless.record._`, never both record imports.
- With record syntax in scope, `apply`, `updateWith` and `replace` on any `HList` are the record versions, which take a key. To use the `HList` versions there, call `formless.hlist.HList.toHListOps(list).updateWith(...)`.
- Build a record field with `"key" ->> value`; a record is an `HList` of fields. Keys are singleton types: string, int, long, char or boolean literals, or a stable object.
- Each operation is a type class (`Selector`, `Updater`, `Remover`, `Modifier`, `Renamer`, `LacksKey`, ...) with syntax on `HList` and record values. The result type is the type class's `Out` member; summon one with its companion's `apply`, e.g. `Selector[R, "a"]`, to see it.
- A failed lookup is a compile error that names the record and the key. It means the key isn't there, or it could only be in an abstract part of the type (a type parameter, abstract type member, or an element whose type isn't known yet). Make the type concrete at that point, or pass the evidence (`using Selector[T, "a"]`) in from where it is concrete.
- Working on formless itself? [`AGENTS.md`](AGENTS.md) describes the build, the source layout and the conventions.
- On Scala 3 the source is in `core/src/main/scala-3/formless`, one file per type class; `record/Lookup.scala` holds the field lookup. The tests in `core/src/test` show every operation.

## Contributing

formless builds with sbt and [sbt-projectmatrix](https://github.com/sbt/sbt-projectmatrix). `sbt core3/test` tests Scala 3 on the JVM, `sbt core/test` Scala 2.13, and `sbt test` every platform and version. The README is generated from `docs/README.md` with `sbt docs3/mdoc`.

## License

formless is licensed under the [Apache License 2.0](LICENSE).
