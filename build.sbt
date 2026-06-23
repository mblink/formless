import formless._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.18"
lazy val scala3 = "3.8.4"
lazy val scala3Next = "3.8.4"
lazy val scala3NextAxis = new VirtualAxis.WeakAxis {
  val idSuffix = "Next"
  val directorySuffix = "next"
}
lazy val scalaVersions = Seq(scala213, scala3)

ThisBuild / scalaVersion := scala3
ThisBuild / version := "0.8.0"

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21, 25).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main")

val isJava25 = s"matrix.java == '${javaVersions.find(_.version == "25").get.render}'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava25)),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava25)),
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val mavenRepoUrl = "https://maven.bondlink-cdn.com"

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

baseSettings

lazy val publishSettings = Seq(
  publish / skip := false,
  s3PublishBucket := "bondlink-maven-repo",
  resolvers += "bondlink-maven-repo" at mavenRepoUrl,
  mimaPreviousArtifacts := Set("com.bondlink" %%% name.value % "0.6.0"),
)

lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.3.3" % Test)
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.13")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0" % Test)

// Newer versions of Scala 3 require Java 17 so we can't use Java 8 or 11 with them
def maybeAddScala3Next(matrix: sbt.internal.ProjectMatrix) = {
  val jv = sys.props.getOrElse("java.specification.version", "")
  if (jv == "1.8" || jv == "8" || jv == "11") matrix
  else matrix.jvmPlatform(
    scalaVersions = Seq(scala3Next),
    axisValues = Seq(scala3NextAxis),
    settings = Seq(
      publish / skip := true,
      mimaPreviousArtifacts := Set(),
    ),
  )
}

lazy val core = maybeAddScala3Next(projectMatrix.in(file("core")))
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)
  .nativePlatform(scalaVersions = scalaVersions)
  .settings(baseSettings)
  .settings(publishSettings)
  .settings(
    name := "formless",
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

lazy val docs = projectMatrix.in(file("formless-docs"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    mdocVariables ++= Map(
      "VERSION" -> version.value,
      "BL_MAVEN_REPO_URL" -> mavenRepoUrl,
      "SHAPELESS_NAT_PREFIX" -> foldScalaV(scalaVersion.value)("shapeless.nat._", ""),
    ),
    scalacOptions -= "-Werror",
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
