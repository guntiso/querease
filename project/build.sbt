resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "11.1.2",
  "org.mojoz"  %% "mojoz"  % "4.1.0",
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures",
  b / ".." / "test" / "macros",
)).value
