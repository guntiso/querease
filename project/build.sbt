resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.3",
  "org.mojoz"  %% "mojoz"  % "5.3.3",
 ("org.tresql" %% "tresql" % "12.0.0").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
