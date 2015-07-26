lazy val root = (project in file(".")).settings(
  organization := "ircd_conformance",
  name := "ircd-conformance",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  // For Scala 2.11+, parsers live in a separate library.
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.+"),
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.3.+",
    "com.typesafe.akka" %% "akka-testkit" % "2.3.+",
    "org.scalatest" %% "scalatest" % "2.2.+",
    "org.scalacheck" %% "scalacheck" % "1.12.+"))
