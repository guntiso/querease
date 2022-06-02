resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "11.1.0-SNAPSHOT",
  "org.mojoz"  %% "mojoz"  % "4.2.0-SNAPSHOT",
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures",
  b / ".." / "test" / "macros",
)).value
