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

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt perf3/test"), name = Some("Check compile-time performance"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt coverage core3/test core3/coverageReport"), name = Some("Check test coverage"), cond = Some(isJava25)),
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
