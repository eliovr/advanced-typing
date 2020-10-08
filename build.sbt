ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "se.his"

lazy val hello = (project in file("."))
  .settings(
    organization := "se.his",
    name := "advanced-typing",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
    scalacOptions += "-deprecation"
  )
