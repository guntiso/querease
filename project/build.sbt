resolvers ++= Seq(
  "snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
)

libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.6",
  "org.mojoz"  %% "mojoz"  % "7.1.0",
 ("org.tresql" %% "tresql" % "13.4.0-SNAPSHOT").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
