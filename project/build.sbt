resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.mojoz"  %% "mojoz"  % "4.2.0",
 ("org.tresql" %% "tresql" % "11.2.0").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "function-signatures",
  b / ".." / "test" / "macros",
)).value
