scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

scalacOptions ++= Seq("-feature", "-deprecation")

scalariformSettings

enablePlugins(JmhPlugin)
