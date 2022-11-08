resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.mojoz"  %% "mojoz"  % "4.2.0",
 ("org.tresql" %% "tresql" % "12.0.0-SNAPSHOT").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
