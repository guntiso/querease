name := "querease"

organization := "org.mojoz"

scalaVersion := "2.11.8"

crossScalaVersions := Seq(
  "2.11.8"
)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % "7.3",
  "org.mojoz" %% "mojoz" % "0.3-SNAPSHOT",
  // test
  "org.hsqldb" % "hsqldb" % "2.3.2" % "test",
  "org.scalatest" %% "scalatest" % "2.1.5" % "test"
)

scalaSource in Compile := baseDirectory(_ / "src").value

autoAPIMappings := true

scalacOptions in (Compile, doc) ++= (baseDirectory in
 LocalProject("querease")).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/querease/blob/develop€{FILE_PATH}.scala")
 }.value

unmanagedResourceDirectories in Test := baseDirectory(b => Seq(b / "sample" / "md", b / "test" / "data")).value

scalaSource in Test := baseDirectory(_ / "test").value

sourceGenerators in Test += Def.task {
    // TODO val cacheDirectory = streams.value.cacheDirectory
    val resDirs: Seq[File] = (unmanagedResourceDirectories in Test).value
    val outDir: File = (sourceManaged in Test).value
    import querease._
    import mojoz.metadata._
    import mojoz.metadata.in._
    import mojoz.metadata.out._
    val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toSeq
    val tableMd = new TableMetadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefs = YamlViewDefLoader(tableMd, yamlMd, TresqlJoinsParser).viewDefs
    object ScalaBuilder extends ScalaClassWriter {
      override def scalaClassTraits(viewDef: ViewDef.ViewDefBase[FieldDef.FieldDefBase[Type]]) =
        if (viewDef.fields.exists(f => f.name == "id" && f.type_.name == "long"))
          List("DtoWithId")
        else List("Dto")
    }
    val file = outDir / "Dtos.scala"
    val contents = ScalaBuilder.createScalaClassesString(
      List("package dto", "",
        "import querease._", ""), viewDefs, Nil)
    IO.write(file, contents)
    Seq(file) // FIXME where's my cache?
}.taskValue

initialCommands in console := "import org.tresql._; import querease._; import mojoz.metadata._"

publishTo := version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}.value

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/MIT"))

pomExtra := (
  <url>https://github.com/guntiso/querease</url>
  <scm>
    <url>git@github.com:guntiso/querease.git</url>
    <connection>scm:git:git@github.com:guntiso/querease.git</connection>
  </scm>
  <developers>
    <developer>
      <id>guntiso</id>
      <name>Guntis Ozols</name>
      <url>https://github.com/guntiso/</url>
    </developer>
    <developer>
      <id>mrumkovskis</id>
      <name>Martins Rumkovskis</name>
      <url>https://github.com/mrumkovskis/</url>
    </developer>
  </developers>
)
