name := "scrimini"

version := "0.1"

scalaVersion in ThisBuild := "2.11.12"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

lazy val root = (project in file(".")).aggregate(interpreter, app)

lazy val interpreter = project.enablePlugins(ScalaNativePlugin)
lazy val app = project.dependsOn(interpreter).enablePlugins(ScalaNativePlugin)

addCommandAlias("app", "; app/run")
