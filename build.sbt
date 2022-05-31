name := "querease"

organization := "org.mojoz"

val scalaV = "2.13.8"

scalaVersion := scalaV

crossScalaVersions := Seq(
  scalaV,
  "2.12.15",
)

ThisBuild / sbt.Keys.versionScheme := Some("semver-spec")
ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

val tresqlV = "11.1.0-SNAPSHOT"
val mojozV  = "4.1.0-SNAPSHOT"
libraryDependencies ++= Seq(
  "org.tresql" %% "tresql" % tresqlV,
  "org.mojoz"  %% "mojoz"  % mojozV,
  // test
  "org.hsqldb"     % "hsqldb"     % "2.3.1"  % "test", // TODO upgrade hsqldb when supported by tresql
  "com.typesafe"   % "config"     % "1.4.1"  % "test",
  "org.scalatest" %% "scalatest"  % "3.2.10" % "test",
  "org.postgresql" % "postgresql" % "42.3.1" % "test",
)

Compile / scalaSource := baseDirectory(_ / "src").value

Compile / unmanagedSourceDirectories ++= {
  val sharedSourceDir = (ThisBuild / baseDirectory).value / "compat"
  if (scalaVersion.value.startsWith("2.12."))
    Seq(sharedSourceDir / "scala-2.12")
  else Nil
}

Compile / doc / scalacOptions ++= (
 LocalProject("querease") / baseDirectory).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/querease/blob/develop€{FILE_PATH}.scala")
 }.value

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

Test / unmanagedResourceDirectories := baseDirectory(b => Seq(
  b / "test" / "conf",
  b / "test" / "data",
  b / "test" / "tables",
  b / "test" / "views"
)).value

Test / scalaSource := baseDirectory(_ / "test").value
Test / unmanagedSources / excludeFilter := "*-out*.*"

Test / unmanagedClasspath +=
  // Needs [pre-compiled] function signatures to compile tresql in generated sources
  baseDirectory.value / "project" / "target" / "scala-2.12" / "sbt-1.0" / "classes"

Test / scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8",
  "-Xmacro-settings:" + List(
    "metadataFactoryClass=org.mojoz.querease.TresqlMetadataFactory",
    "tableMetadataFile=" + new File(((Test / resourceManaged).value / "tresql-table-metadata.yaml").getAbsolutePath).getCanonicalPath,
    "functions=test.FunctionSignatures",
    "macros=test.QuereaseTestMacros"
  ).mkString(", ")
)

Test / resourceGenerators += Def.task {
  import org.mojoz.querease._
  import org.mojoz.metadata._
  import org.mojoz.metadata.in._
  import org.mojoz.metadata.out._
  val resDirs: Seq[File] = (Test / unmanagedResourceDirectories).value
  val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toList
  val file = new File(((Test / resourceManaged).value / "tresql-table-metadata.yaml").getAbsolutePath)
  val contents = new TresqlMetadata(new YamlTableDefLoader(yamlMd).tableDefs.sortBy(_.name)).tableMetadataString
  IO.write(file, contents)
  Seq(file)
}.taskValue

Test / sourceGenerators += Def.task {
    // TODO val cacheDirectory = streams.value.cacheDirectory
    val resDirs: Seq[File] = (Test / unmanagedResourceDirectories).value
    val outDir: File = (Test / sourceManaged).value
    import org.mojoz.querease._
    import org.mojoz.metadata._
    import org.mojoz.metadata.in._
    import org.mojoz.metadata.out._
    val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toList
    val tableMd = new TableMetadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefLoader = YamlViewDefLoader(tableMd, yamlMd,
      new TresqlJoinsParser(new TresqlMetadata(tableMd.tableDefs)))
    val plainViewDefs = viewDefLoader.plainViewDefs
    val xViewDefs = viewDefLoader.nameToViewDef
    val qe = new Querease with ScalaDtoQuereaseIo {
      override lazy val tableMetadata = tableMd
      override lazy val nameToViewDef = xViewDefs.asInstanceOf[Map[String, ViewDef]]
    }
    val ScalaBuilder = new ScalaDtoGenerator(qe) {
      override def scalaClassName(name: String) =
        name.split("[_\\-\\.]+").toList.map(_.toLowerCase.capitalize).mkString
    }
    val file = outDir / "Dtos.scala"
    val contents = ScalaBuilder.generateScalaSource(
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
      plainViewDefs,
      Nil)
    IO.write(file, contents)
    Seq(file) // FIXME where's my cache?
}.taskValue

console / initialCommands := "import org.tresql._; import org.mojoz.querease._; import org.mojoz.metadata._"

Test / console / initialCommands := Seq(
  "import dto._",
  "import org.tresql._",
  "import org.mojoz.querease._",
  "import org.mojoz.metadata._",
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

Test / publishArtifact := false

Test / testOptions += Tests.Argument("-oF")

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
