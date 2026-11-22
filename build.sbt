import formless._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.18"
lazy val scala3 = "3.3.8"
lazy val scala3Next = "3.8.4"
lazy val scala3NextAxis = new VirtualAxis.WeakAxis {
  val idSuffix = "Next"
  val directorySuffix = "next"
}
lazy val scalaVersions = Seq(scala213, scala3)

ThisBuild / scalaVersion := scala3
ThisBuild / version := "0.9.0"

// GitHub Actions config
// formless builds and is tested on Java 17 and newer; the checks that need only one Java version run on the newest
val java25 = JavaSpec.temurin("25")
val javaVersions = Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"), java25)

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main")

val isJava25 = s"matrix.java == '${java25.render}'"

// The Linux CI compiler needs extra stack to compare the two arms of the long record union in LookupTests.
val sbtForTestCompile = "sbt -J-Xss2m"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List(s"$sbtForTestCompile test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt perf3/test"), name = Some("Check compile-time performance"), cond = Some(isJava25)),
  WorkflowStep.Run(List(s"$sbtForTestCompile coverage core3/test core3/coverageReport"), name = Some("Check test coverage"), cond = Some(isJava25)),
  WorkflowStep.Run(List(s"$sbtForTestCompile findUnusedAll"), name = Some("Check for unused code"), cond = Some(isJava25)),
)

// Pushing a tag `vX.Y.Z` matching `version` publishes to Maven Central; see "Publishing" in AGENTS.md
ThisBuild / githubWorkflowTargetTags := Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Run(
    List(
      """test "${GITHUB_REF_NAME}" = "v$(sbt -batch -error 'print core3/version' | tail -n 1)" || { echo "tag ${GITHUB_REF_NAME} doesn't match the build's version"; exit 1; }""",
      "echo \"$PGP_SECRET\" | base64 --decode | gpg --batch --import",
      "sbt publishCentral",
    ),
    name = Some("Publish to Maven Central"),
    env = Map(
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
    ),
  ),
)

// Signs and stages every published artifact, then uploads the bundle to the Central Portal and releases it. sbt reads the
// Central Portal credentials from SONATYPE_USERNAME and SONATYPE_PASSWORD itself.
addCommandAlias("publishCentral", "; set every publishTo := localStaging.value ; publishSigned ; sonaRelease")

ThisBuild / organizationName := "BondLink"
ThisBuild / organizationHomepage := Some(url("https://bondlink.com"))
ThisBuild / homepage := Some(url("https://github.com/mblink/formless"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/mblink/formless"), "scm:git:git@github.com:mblink/formless.git"))
ThisBuild / developers := List(Developer("mblink", "BondLink", "", url("https://github.com/mblink")))
ThisBuild / description := "HLists, extensible records and type-level generic programming for Scala 3 and Scala 2.13"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / pomIncludeRepository := (_ => false)

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val mavenRepoUrl = "https://maven.bondlink-cdn.com"

// find-unused analyzes one Scala version, from 3.8 on, so it reads only the Scala 3 Next build of `core`, whose tests
// count as uses: what it reports is code that neither the library nor its tests use. The Scala 3 LTS sources are the
// same files, and the Scala 2.13 ones are mostly shapeless wrappers.
Global / findUnusedPackages := Seq("formless")

Global / findUnusedExclusions := {
  def companionApply(pkg: String, companions: String*): Seq[String] =
    companions.map(name => s"formless.$pkg.$name.apply")

  // Public summoners for callers who need the type class instance. The operation tests exercise their givens.
  val summoners = companionApply(
    "hlist",
    "Align", "Collect", "CollectFirst", "ConstMapper", "Diff", "Drop", "FlatMapInterleave", "FlatMapper", "Init",
    "Interleave", "Intersection", "Last", "LeftFolder", "LeftReducer", "LeftScanner", "Length", "MapCons", "MapFolder",
    "Mapper", "ModifierAt", "NotContains", "PadTo", "Partition", "Patcher", "Permutations", "Prepend", "Range",
    "RemoveAll", "Repeat", "ReplaceAt", "Reverse", "ReversePrepend", "ReverseSplit", "RightFolder", "RightReducer",
    "RightScanner", "RotateLeft", "RotateRight", "SelectAll", "SelectRange", "Slice", "Split", "SubtypeUnifier", "Take",
    "ToTraversable", "Transposer", "Tupler", "Unifier", "Union", "Unzip", "Zip", "ZipApply", "ZipOne", "ZipWith",
    "ZipWithIndex", "ZipWithKeys",
  ) ++ companionApply(
    "record",
    "Extractor", "Fields", "FromMap", "Keys", "LacksKey", "MapValues", "MergeWith", "Merger", "Remove", "Remover",
    "ToMap", "Updater", "Values",
  )

  val retained = Seq(
    // Public alias for the input types of a Poly2 case.
    "formless.hlist.Poly2.Case",
    // The shared package object exposes HList compatibility definitions on Scala 2.13.
    "formless.hlist.package",
    // This match type reduces before the analyzer sees the equality witnesses in HListHelperTests.
    "formless.hlist.IfEq$package.IfEq",
    // The one-argument overload exists to give an error, and its calls occur only in negative test snippets.
    "formless.record.FormlessRecordOps.replace",
    // The analyzer misses symbols referenced only by compile-time snippets.
    "formless.hlist.ElemOpTests.OpaqueTail.O",
    "formless.hlist.ElemOpTests.KA",
    "formless.hlist.ElemOpTests.KB",
    "formless.record.LookupTests.msgR",
    "formless.record.LookupTests.UnionAB",
    // The analyzer misses references reduced away in compile-time type-equality proofs.
    "formless.hlist.ElemOpTests.<init>.sl",
    "formless.hlist.ElemOpTests.<init>.rsl",
    "formless.hlist.ElemOpTests.<init>.sr",
    "formless.hlist.ElemOpTests.<init>.rsr",
    "formless.hlist.ElemOpTests.<init>.rm",
    "formless.record.LookupTests.<init>.sa",
    "formless.record.LookupTests.<init>.rm",
    "formless.record.LookupTests.<init>.st",
    "formless.record.LookupTests.<init>.f",
    "formless.record.LookupTests.<init>.alias",
    "formless.record.LookupTests.<init>.viaFieldKey",
    "formless.record.LookupTests.<init>.escaped",
    "formless.record.LookupTests.<init>.su",
    "formless.record.LookupTests.<init>.s",
    "formless.record.LookupTests.<init>.u",
    "formless.hlist.HListTests.IIII",
    "formless.hlist.HListTests.IYII",
    "formless.hlist.HListTests.YYYY",
    // The structural member and its parameter are fixtures for the lookup's treatment of method refinements.
    "formless.record.LookupTests.<init>.Payload.<refinement>.value",
    "formless.record.LookupTests.<init>.Payload.<refinement>.value.i",
  )

  (summoners ++ retained).map { symbol =>
    FindUnusedExclusion(sym = ("^" + java.util.regex.Pattern.quote(symbol) + "$").r)
  }
}

// JVM builds compile against the Java 17 API and emit Java 17 bytecode
lazy val jvmSettings = Seq(tpolecatScalacOptions += org.typelevel.scalacoptions.ScalacOptions.release("17"))

lazy val baseSettings = Seq(
  organization := "com.bondlink",
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.patch)),
    Seq(),
  ),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits", "-Vimplicits-verbose-tree"),
    Seq("-no-indent"),
  ),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions",
    "-Xfatal-warnings",
  ),
  Test / scalacOptions -= "-Wunused:nowarn",
  licenses += License.Apache2,
  publish / skip := true,
  mimaFailOnNoPrevious := false,
  findUnusedFullTestClasspath := (if (scalaVersion.value == scala3Next) findUnusedFullTestClasspath.value else Seq()),
)

lazy val root = project.in(file("."))
  .settings(baseSettings)
  .aggregate((core.projectRefs ++ docs.projectRefs): _*)

lazy val publishSettings = Seq(
  publish / skip := false,
  s3PublishBucket := "bondlink-maven-repo",
  resolvers += "bondlink-maven-repo" at mavenRepoUrl,
  // 0.9.0 breaks binary compatibility with earlier releases; check against it once it is published
  mimaPreviousArtifacts := Set(),
)

lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.3.5" % Test)
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.13")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0" % Test)

lazy val core = projectMatrix.in(file("core"))
  // Tested against Scala 3 Next too, but published only for the LTS
  .jvmPlatform(
    scalaVersions = Seq(scala3Next),
    axisValues = Seq(scala3NextAxis),
    settings = jvmSettings ++ Seq(
      publish / skip := true,
      mimaPreviousArtifacts := Set(),
    ),
  )
  .jvmPlatform(scalaVersions = scalaVersions, settings = jvmSettings)
  .jsPlatform(scalaVersions = scalaVersions)
  .nativePlatform(scalaVersions = scalaVersions)
  .settings(baseSettings)
  .settings(publishSettings)
  .settings(
    name := "formless",
    // `sbt coverage core3/test core3/coverageReport` fails below these. The lookup macros count too: they run in the
    // compiler, with the instrumented classes, while the tests compile.
    coverageMinimumStmtTotal := 90,
    coverageMinimumBranchTotal := 82,
    coverageFailOnMinimum := true,
    libraryDependencies ++= Seq(munit.value, scalacheck.value),
    libraryDependencies ++= foldScalaV(scalaVersion.value)(
      Seq(
        shapeless.value,
        scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
      ),
      Seq(),
    ),
    Test / sourceGenerators += Def.task {
      val srcManaged = (Test / sourceManaged).value / "generated"

      def gen(scalaF: String, generator: SourceGenerator) = {
        println(s"Generating ${srcManaged / scalaF} with $generator")
        IO.write(srcManaged / scalaF, generator())
        srcManaged / scalaF
      }

      Seq(
        gen("Util.scala", SourceGenerator.Util),
        gen("HListSelectorTest.scala", SourceGenerator.HListSelectorTest),
        gen("RecordSelectorTest.scala", SourceGenerator.RecordSelectorTest),
        gen("UpdaterTest.scala", SourceGenerator.UpdaterTest),
        gen("ModifierTest.scala", SourceGenerator.ModifierTest),
        gen("RenamerTest.scala", SourceGenerator.RenamerTest),
        gen("RemoverTest.scala", SourceGenerator.RemoverTest),
      )
    },
  )

// Compile-time regression tests: compiles generated workloads and checks the memory the compiler allocates. Not part of
// `sbt test`; CI runs `sbt perf3/test` on one Java version.
lazy val perf = projectMatrix.in(file("perf"))
  .jvmPlatform(scalaVersions = Seq(scala3), settings = jvmSettings)
  .settings(baseSettings)
  .settings(
    libraryDependencies ++= Seq(munit.value, scalaOrganization.value %% "scala3-compiler" % scalaVersion.value % Test),
    Test / fork := true,
    Test / javaOptions ++= Seq("-Xmx2G", "-Xss8m"),
  )
  .dependsOn(core)

lazy val docs = projectMatrix.in(file("formless-docs"))
  .jvmPlatform(scalaVersions = Seq(scala3), settings = jvmSettings)
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    mdocVariables ++= Map(
      "VERSION" -> version.value,
      "SHAPELESS_NAT_PREFIX" -> foldScalaV(scalaVersion.value)("shapeless.nat._", ""),
    ),
    scalacOptions -= "-Werror",
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
