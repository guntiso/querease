resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.mojoz"  %% "mojoz"  % "4.3.0",
  "org.tresql" %% "tresql" % "11.2.2",
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures",
  b / ".." / "test" / "macros",
)).value
