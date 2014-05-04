name := "querease-sample"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-deprecation", "-feature")

retrieveManaged := true

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "default" %% "querease" % "0.1-SNAPSHOT",
  "com.typesafe" % "config" % "1.2.0",
  // test
  "org.scalatest" % "scalatest_2.10" % "2.0.M8" % "test"
)

scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedResourceDirectories in Compile <<= baseDirectory(b => Seq(
  b / "conf",
  b / "md"
))

resourceGenerators in Compile <+= (unmanagedResourceDirectories in Compile, resourceManaged in Compile) map {
    (resDirs: Seq[File], outDir: File) =>
  val file = outDir / "-md-files.txt"
  def recursiveListFiles(f: File): Array[File] = {
    val these = Option(f.listFiles) getOrElse Array()
    these.filter(!_.isDirectory) ++
      these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def mdFiles = resDirs.flatMap(recursiveListFiles).toSeq.map(_.getName).filter(_ endsWith ".yaml")
  val contents = mdFiles.mkString("", "\n", "\n")
  IO.write(file, contents)
  Seq(file)
}

sourceGenerators in Compile <+= (cacheDirectory, unmanagedResourceDirectories in Compile, sourceManaged in Compile) map {
      (cache: File, resDirs: Seq[File], outDir: File) => {
    import querease._
    import mojoz.metadata._
    import mojoz.metadata.in._
    import mojoz.metadata.out._
    val yamlMd = resDirs.map(_.getAbsolutePath).flatMap(YamlMd.fromFiles(_)).toSeq
    val tableMd = new Metadata(new YamlTableDefLoader(yamlMd).tableDefs)
    val viewDefs = (new YamlViewDefLoader(tableMd, yamlMd) with TresqlJoinsParser).viewDefs
    val i18nRules = I18nRules.suffixI18n(Set("_eng", "_rus"))
    val metadata = new Metadata(tableMd.tableDefs, viewDefs, i18nRules)
    object ScalaBuilder extends ScalaClassWriter {
      override def scalaClassTraits(viewDef: ViewDef[Type]) =
        if (viewDef.fields.exists(f => f.name == "id" && f.type_.name == "long"))
          List("DtoWithId")
        else List("Dto")
    }
    val file = outDir / "dto" / "Dtos.scala"
    val contents = ScalaBuilder.createScalaClassesString(
      List("package dto", "",
        "import querease._", ""), metadata.viewDefs, Nil)
    IO.write(file, contents)
    Seq(file) // FIXME where's my cache?
  }
}
