resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "10.1.3-SNAPSHOT",
  "org.mojoz"  %% "mojoz"  % "4.0.0-SNAPSHOT",
)

unmanagedSourceDirectories in Compile := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures"
)).value
