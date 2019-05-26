import sbt.Keys._
import sbt._

name := "StochasticProcessesSoftware"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.8"

lazy val root = (project in file("."))
  .settings(name := "root")
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathIO" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathArray" % "1.0",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.6"
  )


