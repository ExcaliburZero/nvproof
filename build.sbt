name := "nvproof"

version := "1.0"

scalaVersion := "2.12.1"
sbtVersion := "0.13.13"

resolvers += Opts.resolver.sonatypeSnapshots

mainClass in Compile := Some("nvproof.Main")

// Libraries
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// Linting
resolvers += Resolver.sonatypeRepo("snapshots")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
