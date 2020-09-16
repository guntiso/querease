resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "10.0.0",
  "org.mojoz" %% "mojoz" % "2.0.0-SNAPSHOT"
)

unmanagedSourceDirectories in Compile := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures"
)).value
