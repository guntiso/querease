libraryDependencies ++= Seq(
  "com.typesafe"% "config" % "1.4.9",
  "org.mojoz"  %% "mojoz"  % "7.2.0",
 ("org.tresql" %% "tresql" % "13.5.1").exclude(
  "org.scala-lang.modules",   "scala-parser-combinators_2.12"),
)

Compile / unmanagedSourceDirectories := baseDirectory(b => Seq(
  b / ".." / "src",
  b / ".." / "test" / "macros",
)).value
