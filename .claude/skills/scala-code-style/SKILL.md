---
name: scala-code-style
description: How to write, test and review Scala code in formless — compiler-driven development, type safety, immutability, validating at boundaries, errors as values, DRY, and verification. Use whenever writing, changing, testing or reviewing Scala code in this repository, including macros.
---

# Scala code style for formless

Apply these to all new and changed code, macros included. When touching existing code that breaks them, fix it if the change is small and local, otherwise mention it.

## Compiler-driven development

Push correctness into the types so the compiler rejects wrong code, and let it drive the work:

1. Change the type first. Read *all* the compiler errors, then fix them in dependency order: types, then helpers, then call sites. Finish with a full verification (see Verification).
2. Every fix is semantically complete, never a silencer. No `.asInstanceOf` to make an error go away, no catch-all `case _`, no blanket `@nowarn`. Fix unused-value or value-discard warnings by removing or consuming the value.
3. Carry the principled type (`Option`, `Either`, an ADT, a tagged type) to the last point that can use it, and convert only where a consumer can't accept it. Never collapse early to a sentinel. If the honest type breaks a consumer, fix the consumer.
4. Verify type-system claims with the compiler, not memory: variance, implicit resolution, whether a bound or tag enforces anything. Prove it in a scratch file or a `typeCheckErrors` test.
5. When a bug turns up, ask whether it can become a type error. If it can, fix the type rather than adding a check.

## Make illegal states unrepresentable

- Model alternatives as ADTs (`enum`, or `sealed trait` with `final case class` / `case object` cases), never as strings, magic numbers, sentinel values or `Boolean`s standing for two modes. For example `Updater` chooses between `FieldUpdate.Replace(index)` and `FieldUpdate.Append`, not an index of `-1`.
- Use sealed intermediate classes rather than union aliases to classify cases, so a new case has to pick its parent.
- Never compare strings to decide behavior: no `getClass.getName.endsWith(...)`, no matching on message text. Compare typed values or classes. Strings are for messages people read.
- Literal values are compared as typed ADT values (see `Lookup.LiteralKey`), so `1`, `1L` and `'1'` stay distinct without `getClass` checks.
- Bound type parameters that act as tags by a sealed parent, e.g. `Op <: FieldOp.Op`, so an unsupported operation doesn't type-check.
- Parameters must not admit invalid states. When one parameter's valid values depend on another's (an index into a list, a key of a record, a field of an object), don't take them separately: take one value that carries both, built where the relationship was established, e.g. an evidence object that pairs a type with an index proven in range for it, or derive the dependent value from the other inside the function. If a value can be computed from the others (a length, an index that equals a list's size), compute it rather than store or pass it. A runtime check that rejects a combination of parameters, or a branch for a combination that "can't happen", means the parameters are badly shaped.
- Take the narrowest type a function needs (`NonEmptyList` where non-empty is required).
- No casts (`asInstanceOf`) unless absolutely necessary. A cast is necessary only where the compiler can't relate a value to a type that is nonetheless proven: a runtime `HList` built by an operation and the result type its lookup macro or match type computed, an element read out of an erased list, a tagged type that is an intersection, and Scala 2 macro output. Even there, put the cast in one helper rather than in every caller. Never cast to share one instance between type arguments (allocate, or use a non-capturing lambda, which the JVM caches), to make a value of an opaque type outside its scope (add a method inside the scope), to restate a type a pattern already binds, to widen a type parameter's bound you could declare instead, or to silence an error.
- Absence is `Option`, never `null` or an empty string. Wrap a possibly-null Java value where it enters, with `Option(x)`.
- Match exhaustively over sealed types and enums: list every case, never `case _`. A wildcard is fine only when the scrutinee is genuinely open (a `Type` in a macro, strings, numbers).

## Prefer immutability

- Use `val`, immutable collections, and recursion or folds by default.
- Recursion whose depth grows with the input (a list's length, a record's fields, a long union) must be stack-safe: tail-recursive and annotated `@annotation.tailrec` so the compiler enforces it, or a loop or fold. Non-tail recursion is fine only when its depth is bounded by something small, such as how deeply a type is nested, and a comment says so.
- Mutable state (`var`, arrays, builders) is allowed only where it measurably matters for performance. It must stay local to one method or one instance, with a comment saying why. Examples: the runtime split in `HList.splitAt`, and the walk's step budget in `Lookup.Walk`.

## Validate at boundaries, then trust the types

- Check input once, where it enters (a macro's entry point, a public constructor, a parse step), and produce a type that encodes the result. Code past the boundary relies on that type instead of re-checking.
- Prefer building the result in the same pass that validates it, so later steps can't fail. For example `Walk.removedReinsertably` checks that `reinsert` can rebuild the list and builds the removed type together, and `Found.rest` carries the tail a split needs instead of walking the type again.
- Don't sprinkle defensive checks (`require`, `throw`, `getOrElse(sys.error(...))`) through internal code for states the types already rule out.
- In the lookup macros, each macro's entry point (`fieldOpImpl`, `fieldUpdateImpl`, `elemOpImpl`, ...) goes through `Lookup.guarded`, the only place that reports a compile error.

## Errors are values

- Return failures as values in a principled type: `Either[E, A]` with a sealed error ADT `E` (see `Lookup.LookupError` and `Lookup.Reason`); `Option` when there's nothing to say; accumulate when several independent errors should all be reported.
- Don't escape control flow from deep inside a computation: no `throw`, `return` or `report.errorAndAbort` below the boundary. Thread the `Either` out and decide at the boundary.
- Don't write literal `try`/`catch`. Capture exceptions as values with `scala.util.Try` or `scala.util.control.Exception` (`allCatch.either`, `catching(...)`), then convert them into the error ADT. `Try` doesn't catch fatal errors such as `StackOverflowError`; use `scala.util.control.Exception` when those must be captured.
- Don't throw away an error with `Try(...).toOption` unless absence really is the right answer. When it falls back, the fallback must be the conservative one. For example, a type bound that can't be read is treated as possibly abstract, not as known.
- Exceptions that are another system's control flow (the compiler's `StopMacroExpansion` and its suspension) pass through unchanged; identify them by class, not by name.

## Least power, and no ambient defaults

- Use the cheapest tool that rules out the bug: plain givens, then `Mirror` and `scala.compiletime` ops, then quoted macros. Type-level machinery costs compile time on every user's build, so add no given, overload or derivation that nothing uses.
- A missing instance or a default parameter must never silently mean "do nothing". A parameter whose value matters gets no default.
- Require only the evidence you use, and none the compiler can derive.
- Public syntax and type class instances may require only public evidence. Generic user code passes evidence explicitly (`def f[R <: HList](r: R)(using s: Selector[R, K])`), so requiring a `private[formless]` type it can't name breaks it at compile time. Changing which evidence a public method requires is a source break: check it against BondLink before merging.
- Name escape hatches `unsafe*` (e.g. `unsafeApply`) so they're greppable. Every evidence type gets an `@implicitNotFound` message saying what's wrong and how to fix it. Syntax methods summon evidence as `TC[A, B] { type Out = O }`, not through an `Aux` alias, which the message can't see through.

## Scala 3 and cross-building idioms

- Use `extension` methods, not `implicit class`; no new implicit conversions.
- Use `@targetName`, not `DummyImplicit`, for overloads with the same erasure. Use `TypeTest`, not `ClassTag`.
- Mark anything used only in its file `private`, and members subclasses mustn't override `final`.
- Declare `val`s and givens above their first use. Never read an abstract `val` from a concrete one during initialization.
- In derivation, summon per-field instances lazily, and keep a recursive ADT as a test case.
- The public API is the same on Scala 2.13 and 3, so a new operation needs both implementations and shared tests.

## DRY

- Search before you write: grep for an existing helper first. If there's none, add it once in its logical owner, not inline.
- Remove duplicated *knowledge* (the same rule, magic value or type shape), not code that is merely similar but means something different. Values that are only coincidentally equal stay separate.
- Abstract identical logic at two instances, and structurally similar code at three; when in doubt, keep the duplication. A third occurrence is extracted in the change that adds it, and the earlier call sites move to it in the same change.
- A type shape, function type or constant written twice becomes one named alias or `val`. Near-identical functions become one parameterized function.
- Derive a list of members and the type of the same members from one source instead of restating them.
- Utilities keep generic names and parameters; domain specifics belong in the caller.
- Removing a feature removes everything it orphans, in the same change. Run find-unused with `sbt findUnusedAll` to check.

## Tests

- Compile-time behavior is tested with `scala.compiletime.testing.typeCheckErrors`. Every guard (`@implicitNotFound`, `compiletime.error`, a macro refusal) gets a negative test pinning its message, so a guard that stops firing is noticed.
- Don't test what the types already make impossible. Each test must fail when its target breaks and use only reachable states. One test per rule; deleting a weak or redundant test is a valid change.
- Use an example test for a specific case, one on each side of a boundary. Use a scalacheck property over shared `Arbitrary`s for invariants and laws.
- Use literal inputs and literal expected outputs, with `assertEquals(actual, expected)` and `fail("reason")`. Shared assertion helpers take `using munit.Location`.
- No `scala.util.Random`, no `suchThat` or filters in generators (generate valid values directly), and no `.ignore`, `.flaky` or `assume`.

## Verification

- Make no completion claim without fresh command output whose exit code you've read. Run `sbt core3/test coreNext3/test coreLatest3/test core/test linkage/test` at least, and `sbt test` for anything touching shared or platform code. Lookup macro changes need a clean build, since zinc keeps stale macro expansions.
- "Bug fixed" means the failure was reproduced, then shown gone, then shown to come back when the fix is removed.
- Changing a shared helper means running its owner's tests and checking every call site.
- Say what the evidence doesn't prove.

## Debugging

- Rule out the environment first (stale build, wrong branch, machine load); retrying until it's green is not a fix.
- Test one hypothesis at a time with one minimal change, revert it if it misses, and keep a log of attempts. After three misses, stop and question the type design. Fix the root cause, types first.

## Changes

- Keep the diff to the change: no unrelated renames or reformatting. A pure refactor is its own commit or PR.
- Rename only when the meaning changes, and don't let a name promise more than the code does.
