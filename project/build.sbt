resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "10.0.0-SNAPSHOT",
  "org.mojoz" %% "mojoz" % "1.2.1"
)

unmanagedSourceDirectories in Compile := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures"
)).value
