resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "8.5-SNAPSHOT",
  "org.mojoz" %% "mojoz" % "0.9-SNAPSHOT"
)

scalaSource in Compile := baseDirectory(_ / ".." / "src").value

unmanagedSources in Compile ~= { _ filter(f =>
  ".*[\\\\/]TresqlMetadata\\.scala".r.pattern.matcher(f.getAbsolutePath).matches ||
  ".*[\\\\/]TresqlJoinsParser\\.scala".r.pattern.matcher(f.getAbsolutePath).matches)
}
