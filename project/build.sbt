resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "6.0-M10-SNAPSHOT",
  "org.mojoz" %% "mojoz" % "0.1-SNAPSHOT"
)

scalaSource in Compile <<= baseDirectory(_ / ".." / "src")

unmanagedSources in Compile ~= { _ filter(f =>
  ".*[\\\\/]TresqlJoinsParser\\.scala".r.pattern.matcher(f.getAbsolutePath).matches)
}
