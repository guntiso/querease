resolvers += "snapshots" at "https://central.sonatype.com/repository/maven-snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.9",
  "org.mojoz"  %% "mojoz"  % "7.2.1",
 ("org.tresql" %% "tresql" % "14.0.0-SNAPSHOT").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
