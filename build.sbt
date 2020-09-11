name := "scrimini"

version := "0.1"

scalaVersion in ThisBuild := "2.11.12"

lazy val root = (project in file(".")).aggregate(interpreter, app)

lazy val interpreter = project.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test")
lazy val app = project.dependsOn(interpreter)

addCommandAlias("app", "; app/run")
