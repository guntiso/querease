resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "11.0.0",
  "org.mojoz"  %% "mojoz"  % "4.0.0",
)

unmanagedSourceDirectories in Compile := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures"
)).value
