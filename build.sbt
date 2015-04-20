name := """imogen"""

version := "1.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.3"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.6"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
