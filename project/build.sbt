resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "6.1-SNAPSHOT",
  "org.mojoz" %% "mojoz" % "0.2-SNAPSHOT"
)

scalaSource in Compile <<= baseDirectory(_ / ".." / "src")

unmanagedSources in Compile ~= { _ filter(f =>
  ".*[\\\\/]TresqlJoinsParser\\.scala".r.pattern.matcher(f.getAbsolutePath).matches)
}
