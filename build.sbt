name := """imogen"""

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.3"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
