resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com"

addSbtPlugin("bondlink" % "sbt-s3-publish" % "0.0.1")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.11.0")
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.32.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.2.1")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.7")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.23.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.4.0")

// Finds code nothing uses; see "Unused code" in AGENTS.md
addSbtPlugin("bondlink" % "find-unused-plugin" % "0.7.0")
