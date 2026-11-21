---
name: remove-unused-code
description: Run find-unused on a Scala 3 project and safely delete what it reports - proving each finding is really unused, checking that implicit resolution didn't change by comparing bytecode, and deciding between deleting, annotating and excluding. Use when find-unused reports findings, when cleaning up dead code, or when editing `findUnusedExclusions`.
---

# Removing unused code with find-unused

find-unused reads the compiled TASTy (plus bytecode for givens, default values and objects) across every module, so it
finds what `-Wunused` (one compilation unit at a time) can't: givens, derived instances, exports and members nothing in
the codebase uses. It can be wrong, so never delete on its word alone. This skill is for a project that *uses*
find-unused; to check a change to find-unused itself, see `validate-on-project`.

## Running it

```bash
sbt findUnusedAll
```

It fails when it finds anything. To capture the table, set `Global / findUnusedOutputFile := Some(file(...))` in a
local, uncommitted `.sbt` file named to sort after `build.sbt` (e.g. `zz-local.sbt`) so it overrides the build's
setting. `table2tsv.sh` (in this folder) turns the table into `name<TAB>location` lines. With an output file
set, find-unused's `[find-unused] ...` progress lines are lost, so leave it unset when you need them.

Exclusions live in `findUnusedExclusions`. Only exclude what's genuinely unused in Scala but must stay: generated
code, values only another language reads, deliberately kept APIs. Avoid broad patterns, which hide real dead code
along with the false positives. An exclusion that matches nothing is stale; `findUnusedAll` lists these after its
results (it checks a `fullSymsExclusion`-style group as a whole, so check an entry inside one by hand). An exclusion
also keeps the objects and classes around what it matches.

## Before deleting anything it reports

1. **Delete it and compile everything** (`sbt Test/compile`). A compile error means it's used: restore it and treat it
   as a find-unused bug (report it at github.com/mblink/find-unused/issues) rather than adding an exclusion.
2. **Check nothing outside Scala reads it.** Values generated into TypeScript, JSON schemas, templates or reflection
   configs aren't checked by the Scala compiler. Search those sources by name; if one reads it, exclude it.
3. **Check that behavior is unchanged.** Removing a given nothing selected can still change implicit resolution
   (unshadowing another given, flipping a `NotGiven`, changing a `summonFrom` branch), and that compiles fine. The
   resolution is compiled into every use site, so compare the bytecode. Before editing, clone the class directories,
   keeping their relative paths (`cp -c -R` clones on macOS; use `cp -R` elsewhere):

   ```bash
   find . -path ./node_modules -prune -o \( -path '*/target/scala-*/classes' -o -path '*/target/scala-*/test-classes' \) -type d -print > "$SCRATCH/dirs.txt"
   while read d; do mkdir -p "$SCRATCH/before/$(dirname "$d")" && cp -c -R "$d" "$SCRATCH/before/$d"; done < "$SCRATCH/dirs.txt"
   ```

   (Adjust the `find` patterns for sbt 2, whose output directories differ, or other build tools.) After recompiling,
   copy `CompareBytecode.scala` (in this folder) to `$SCRATCH` (scala-cli writes `.scala-build/` next to it) and run

   ```bash
   scala-cli run CompareBytecode.scala -- "$SCRATCH/before" <repo root> "$SCRATCH/dirs.txt"
   ```

   It fails if a listed directory is missing from both roots. Only classes compiled from the files you edited may
   change (removed members, `<clinit>` setup for removed lazy vals, `$deserializeLambda$`). A change anywhere else
   means resolution changed: restore and investigate, after ruling out noise:

   - Some macros and auto-derivations generate different bytecode on every compile (e.g. by iterating an unordered
     collection). Append a comment to the suspect file (forcing a recompile without changing its API), recompile, and
     compare with the previous build; if they differ too, it's noise.
   - Incremental compilation changes what a file compiles to depending on which other files were recompiled with it,
     especially when it expands `inline` code from them (dead `MODULE$` stores, `List(a, b)` as a `::` chain or
     `List.apply`, calls to empty trait `$init$`). If this shows up in files you didn't edit, compare two *clean*
     builds instead: `sbt clean Test/compile` with the code before the edit, snapshot, then the same after it.
4. **Clean up after it.** Removing code often leaves unused imports, which `-Werror` rejects.

To remove a derived instance, drop just that type class from the `derives` clause (and the whole clause if it was the
only one). A finding like `Foo.bar(baz = ...)` is a default value no call uses: remove just the ` = ...`
part of the parameter (`scala-cli run RemoveDefaults.scala -- all.tsv` in this folder prints each removal, and
`--apply` makes them; use a run without `--transitive`). A call that relied on the default fails to compile. Macros that read defaults show up in the
bytecode, so find-unused doesn't report those.

## Unused on purpose

Some givens are never selected by design, e.g. a pair of identical givens that makes implicit search ambiguous so that
summoning an unsupported instance fails to compile, or one that overrides a library's default. Deleting them compiles
but silently changes behavior. Keep them and either annotate them with `@unused` or `@nowarn("msg=unused")` (both are
honored) with a comment saying why, or put them in an exclusion group for givens that only steer implicit resolution.

## Known gaps

See the README's known limitations. In short: an `inline given` only resolved inside library derivation is treated as
used (enable the compiler plugin, `findUnusedCompilerPlugin := true`, then `clean`, for exact results); members that
macros look up by name look unused; wildcard exports report every unused forwarder (use `name as _` selectors).
