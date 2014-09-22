name := "crf"

organization := "sam"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

fork in run := true

fork in runMain := true

javaOptions in run += "-Xmx50G"

javaOptions in runMain += "-Xmx50G"

javaOptions in run += "-Dfile.encoding=UTF-8"

javaOptions in runMain += "-Dfile.encoding=UTF-8"
