import formless._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.17"
lazy val scala3 = "3.3.7"
lazy val scala3Next = "3.7.3"

ThisBuild / crossScalaVersions := Seq(scala213, scala3, scala3Next)
ThisBuild / scalaVersion := scala3
ThisBuild / version := "0.7.0"

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21, 25).map(v => JavaSpec.temurin(v.toString))

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

lazy val mavenRepoUrl = "https://maven.bondlink-cdn.com"

lazy val baseSettings = Seq(
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala3, scala3Next),
  organization := "com.bondlink",
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
  resolvers += "bondlink-maven-repo" at mavenRepoUrl,
  mimaPreviousArtifacts := Set("com.bondlink" %%% name.value % "0.6.0"),
  mimaFailOnNoPrevious := false,
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.patch)),
    Seq(),
  ),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits", "-Vimplicits-verbose-tree"),
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
  Test / scalacOptions -= "-Wunused:nowarn",
  licenses += License.Apache2,
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
)

lazy val root = project.in(file("."))
  .aggregate(
    (core.componentProjects ++ Seq(docs)).map(p => p: ProjectReference):_*
  )
  .settings(baseSettings)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)

lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.1" % Test)
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.13")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0" % Test)

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
    // Disable publishing for Scala 3.6
    publish := { if (scalaVersion.value == scala3Next) () else publish.value },
    publishLocal := { if (scalaVersion.value == scala3Next) () else publishLocal.value },
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
