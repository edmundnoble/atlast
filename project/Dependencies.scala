import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.{AbstractJSDep, ScalaJSPlugin}
import sbt.Keys._
import sbt._
import sbt.complete.Parser
//import scoverage.ScoverageKeys.coverageExcludedPackages

object Dependencies {

  val cats = libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "0.9.0",
    "org.typelevel" %%% "cats-kernel" % "0.9.0",
    "org.typelevel" %%% "cats-macros" % "0.9.0",
    "org.typelevel" %%% "cats-free" % "0.9.0"
  )

  val scalatest = libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
  )

  val kindProjector = 
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)

}
