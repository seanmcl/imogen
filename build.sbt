name := """imogen"""

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.2"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.6"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
