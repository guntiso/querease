libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "9.2",
  "org.mojoz" %% "mojoz" % "1.2.1"
)

scalaSource in Compile := baseDirectory(_ / ".." / "src").value

unmanagedSources in Compile ~= { _ filter(f =>
  ".*[\\\\/]TresqlMetadata\\.scala".r.pattern.matcher(f.getAbsolutePath).matches ||
  ".*[\\\\/]TresqlJoinsParser\\.scala".r.pattern.matcher(f.getAbsolutePath).matches)
}
