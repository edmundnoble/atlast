import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.{AbstractJSDep, ScalaJSPlugin}
import sbt.Keys._
import sbt._
import sbt.complete.Parser
//import scoverage.ScoverageKeys.coverageExcludedPackages

object AtlastBuild {

val baseSettings = Seq(
    version := "0.0.1",
    scalaVersion := "2.12.1",
    scalaOrganization := "org.typelevel",
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-Xlint",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-unchecked",
      "-Xfatal-warnings",
      "-Yno-adapted-args",
      "-Ywarn-unused-import",
      "-Ywarn-adapted-args",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Yinduction-heuristics",
      "-Ypartial-unification",
      "-Yliteral-types",
      "-Ywarn-nullary-unit",
      "-Xfuture"
    ),
    scalacOptions in Compile += "-Ywarn-value-discard",
    persistLauncher in Compile := true,
    persistLauncher in Test := false,
    Dependencies.kindProjector
  )

  // amazingly hard to do
  def emptyInputTask: Def.Initialize[InputTask[Unit]] =
    InputTask.createDyn[String, Unit](
      InputTask.parserAsInput(
        Parser.zeroOrMore(
          Parser.charClass(_ => true)).map(_.mkString))
    )(Def.task { (_: String) => Def.task(()) })

  private val disableTests: Seq[Def.Setting[_]] = Seq(
    test in Test := (),
    testQuick in Test := emptyInputTask.inputTaskValue,
    testOnly in Test := emptyInputTask.inputTaskValue
  )

  lazy val core: Project =  project.in(file("core"))
    .settings(baseSettings: _*)
    .settings(Dependencies.cats: _*)
    .settings(Dependencies.scalatest: _*)

  lazy val atlast: Project = project.in(file("."))
    .aggregate(core)
    .settings(Defaults.projectCore)
    .settings(baseSettings: _*)
    .settings(disableTests: _*)

}
