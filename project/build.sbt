resolvers ++= Seq(
  "snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
)

libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.3",
  "org.mojoz"  %% "mojoz"  % "6.0.0-RC3-SNAPSHOT",
 ("org.tresql" %% "tresql" % "13.0.0-RC2-SNAPSHOT").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
