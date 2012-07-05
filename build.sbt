name := "RandomStuff"

version := "1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-full_2.9.1" % "6.0.4",
  "org.scalatest" %% "scalatest" % "1.7.1" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
)