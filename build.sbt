name := "scrimini"

version := "0.1"

scalaVersion in ThisBuild := "3.0.0"

lazy val root = (project in file(".")).aggregate(interpreter, app)

lazy val interpreter = project.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test")
lazy val app = project.dependsOn(interpreter)

addCommandAlias("app", "; app/run")
