resolvers ++= Seq(
  "snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
)

libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.6",
  "org.mojoz"  %% "mojoz"  % "7.0.2",
 ("org.tresql" %% "tresql" % "13.1.0").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
