import sbt._
import Keys._

object MacroBuild extends Build {
  lazy val main = Project("main", file(".")) dependsOn macroSub
  lazy val macroSub = Project("macro", file("macro")) settings(
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )
}