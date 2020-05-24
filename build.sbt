name := "querease"

organization := "org.mojoz"

val scalaV = "2.13.2"

scalaVersion := scalaV

crossScalaVersions := Seq(
  scalaV,
  "2.12.11",
  "2.11.12",
  "2.10.7",
)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

val tresqlV = "10.0.0-SNAPSHOT"
val mojozV = "1.2.1"
libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % tresqlV,
  "org.mojoz" %% "mojoz" % mojozV,
  // test
  "org.hsqldb" % "hsqldb" % "2.3.1" % "test",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

scalaSource in Compile := baseDirectory(_ / "src").value

autoAPIMappings := true
apiMappings ++= {
  val fcp = (fullClasspath in Compile).value
  val scalaMajorV = scalaVersion.value.substring(0, 4)
  val mappings: Map[String, String] =
    fcp.files.map(_.getName).filter(_ startsWith "tresql")
      .map(tresqljar => (tresqljar, s"https://oss.sonatype.org/service/local/repositories/public/archive/org/tresql/tresql_$scalaMajorV/$tresqlV/tresql_$scalaMajorV-$tresqlV-javadoc.jar/!/index.html")).toMap ++
    fcp.files.map(_.getName).filter(_ startsWith "mojoz")
      .map(mojozjar => (mojozjar, s"https://oss.sonatype.org/service/local/repositories/public/archive/org/mojoz/mojoz_$scalaMajorV/$mojozV/mojoz_$scalaMajorV-$mojozV-javadoc.jar/!/index.html")).toMap
  fcp.files.filter(f => mappings.contains(f.getName))
    .map(f => (f, new java.net.URL(mappings(f.getName)))).toMap
}

scalacOptions in (Compile, doc) ++= (baseDirectory in
 LocalProject("querease")).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/querease/blob/develop€{FILE_PATH}.scala")
 }.value

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

unmanagedResourceDirectories in Test := baseDirectory(b => Seq(
  b / "test" / "data",
  b / "test" / "tables",
  b / "test" / "views"
)).value

scalaSource in Test := baseDirectory(_ / "test").value
Test / unmanagedSources / excludeFilter := "*-out*.*"

unmanagedClasspath in Test +=
  // Needs [pre-compiled] function signatures to compile tresql in generated sources
  baseDirectory.value / "project" / "target" / "scala-2.12" / "sbt-1.0" / "classes"

scalacOptions in Test := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8",
  "-Xmacro-settings:" + List(
    "metadataFactoryClass=querease.TresqlMetadataFactory",
    "tableMetadataFile=" + new File(((resourceManaged in Test).value / "tresql-table-metadata.yaml").getAbsolutePath).getCanonicalPath,
    "functions=test.FunctionSignatures",
    "macros=org.tresql.Macros"
  ).mkString(", ")
)

resourceGenerators in Test += Def.task {
  import querease._
  import mojoz.metadata._
  import mojoz.metadata.in._
  import mojoz.metadata.out._
  val resDirs: Seq[File] = (unmanagedResourceDirectories in Test).value
  val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toList
  val file = new File(((resourceManaged in Test).value / "tresql-table-metadata.yaml").getAbsolutePath)
  val contents = new TresqlMetadata(new YamlTableDefLoader(yamlMd).tableDefs.sortBy(_.name)).tableMetadataString
  IO.write(file, contents)
  Seq(file)
}.taskValue

sourceGenerators in Test += Def.task {
    // TODO val cacheDirectory = streams.value.cacheDirectory
    val resDirs: Seq[File] = (unmanagedResourceDirectories in Test).value
    val outDir: File = (sourceManaged in Test).value
    import querease._
    import mojoz.metadata._
    import mojoz.metadata.FieldDef._
    import mojoz.metadata.ViewDef._
    import mojoz.metadata.in._
    import mojoz.metadata.out._
    val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toList
    val tableMd = new TableMetadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefLoader = YamlViewDefLoader(tableMd, yamlMd,
      new TresqlJoinsParser(new TresqlMetadata(tableMd.tableDefs)))
    val viewDefs = viewDefLoader.viewDefs
    val xViewDefs = viewDefLoader.extendedViewDefs
    val qe = new Querease with ScalaDtoQuereaseIo {
      override lazy val tableMetadata = tableMd
      override lazy val viewDefs = xViewDefs.asInstanceOf[Map[String, ViewDef]]
    }
    val ScalaBuilder = new ScalaDtoGenerator(qe) {
      override def scalaClassName(name: String) = Naming.camelize(name)
      override def useTresqlInterpolator =
        !scalaVersion.value.startsWith("2.10.") && // tresql interpolator not available for old scala
        !scalaVersion.value.startsWith("2.11.")    // errors to be resolved?
    }
    val file = outDir / "Dtos.scala"
    val contents = ScalaBuilder.createScalaClassesString(
      List(
        "package dto",
        "",
        "import org.tresql._",
        if  (scalaVersion.value.startsWith("2.10."))
            "import org.tresql.CoreTypes._"
        else null,
        "import test.{ Dto, DtoWithId }",
        "import test.QuereaseDbTests.Env",
        "").filter(_ != null),
      viewDefs,
      Nil)
    IO.write(file, contents)
    Seq(file) // FIXME where's my cache?
}.taskValue

initialCommands in console := "import org.tresql._; import querease._; import mojoz.metadata._"

initialCommands in (Test, console) := Seq(
  "import dto._",
  "import org.tresql._",
  "import querease._",
  "import mojoz.metadata._",
  "import test._",
  "import QuereaseTests._",
  "import QuereaseDbTests._",
  "import QuereaseHsqldbTests._",
).mkString("; ")

publishTo := version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}.value

publishMavenStyle := true

publishArtifact in Test := false

testOptions in Test += Tests.Argument("-oF")

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
