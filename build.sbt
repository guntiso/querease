name := "querease"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature")

retrieveManaged := true

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "5.3",
  "default" %% "mojoz" % "0.1-SNAPSHOT",
  // test
  "org.scalatest" % "scalatest_2.10" % "2.0.M8" % "test"
)
