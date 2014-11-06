name := "sor1k"

organization := "com.pwootage"

version := "1.0b-SNAPSHOT"

isSnapshot := true

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
)

publishTo := Some(Resolver.sftp("My maven", "pwootage.com", "/srv/maven"))