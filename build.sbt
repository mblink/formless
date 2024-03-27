import formless._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.13"
lazy val scala3 = "3.3.3"
lazy val scala3_4 = "3.4.0"

ThisBuild / crossScalaVersions := Seq(scala213, scala3, scala3_4)
ThisBuild / scalaVersion := scala3
ThisBuild / version := "0.3.0-RC1"

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main")

val isJava8 = s"matrix.java == '${javaVersions.find(_.version == "8").get.render}'"
val isScala3 = s"matrix.scala == '$scala3'"

ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava8)),
  WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava8 ++ " && " ++ isScala3)),
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val mavenRepoUrl = "https://raw.githubusercontent.com/mblink/maven-repo/main"

lazy val baseSettings = Seq(
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala3, scala3_4),
  organization := "com.bondlink",
  resolvers += "bondlink-maven-repo" at mavenRepoUrl,
  mimaPreviousArtifacts := Set(),
  mimaFailOnNoPrevious := false,
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.patch)),
    Seq(),
  ),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits-verbose-tree"),
    Seq(
      "-no-indent",
      "-Wunused:unsafe-warn-patvars",
    ),
  ),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions"
  ),
  licenses += License.Apache2,
  gitPublishDir := file("/src/maven-repo")
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  gitRelease := {},
)

lazy val root = project.in(file("."))
  .aggregate(
    (core.componentProjects ++ Seq(docs)).map(p => p: ProjectReference):_*
  )
  .settings(baseSettings)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)

lazy val munit = Def.setting("org.scalameta" %% "munit" % "1.0.0-M11" % Test)
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.10")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.17.0" % Test)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("core"))
  .settings(baseSettings)
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
    // Disable publishing for Scala 3.4 RC
    publish := { if (scalaVersion.value == scala3_4) () else publish.value },
    publishLocal := { if (scalaVersion.value == scala3_4) () else publishLocal.value },
    gitRelease := { if (scalaVersion.value == scala3_4) () else gitRelease.value },
  )
  // Disable scala native on Scala 3.4 RC until fix is released: https://github.com/lampepfl/dotty/issues/19648
  .nativeSettings(
    Compile / sources := { if (scalaVersion.value == scala3_4) Seq() else (Compile / sources).value },
    Test / sources := { if (scalaVersion.value == scala3_4) Seq() else (Test / sources).value },
  )

lazy val docs = project.in(file("formless-docs"))
  .settings(baseSettings)
  .settings(noPublishSettings)
  .settings(
    mdocOut := file("."),
    mdocVariables ++= Map(
      "VERSION" -> version.value,
      "BL_MAVEN_REPO_URL" -> mavenRepoUrl,
      "SHAPELESS_NAT_PREFIX" -> foldScalaV(scalaVersion.value)("shapeless.nat._", ""),
    ),
    scalacOptions -= "-Xfatal-warnings",
  )
  .dependsOn(core.jvm)
  .enablePlugins(MdocPlugin)
  .disablePlugins(MimaPlugin)
