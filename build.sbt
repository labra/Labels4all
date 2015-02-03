name := """Labels4all"""

version := "2.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

libraryDependencies += "com.typesafe.slick" %% "slick" % "2.1.0"