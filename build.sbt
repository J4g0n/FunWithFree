name := "basic-project"

organization := "example"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.4", "2.11.8")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.typelevel" %% "cats" % "0.4.1"
)

//initialCommands := "import example._"
