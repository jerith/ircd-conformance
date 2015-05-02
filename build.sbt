name := "ircd-conformance"

version := "0.1-SNAPSHOT"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.10"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.10"
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
