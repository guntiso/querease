name := "querease-sample"

scalaVersion := "2.10.3"

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

unmanagedResourceDirectories in Compile <<= baseDirectory(b => Seq(
  b / "src" / "main" / "resources",
  b / "conf",
  b / "md"
))

sourceGenerators in Compile <+= (cacheDirectory, unmanagedResourceDirectories in Compile, sourceManaged in Compile) map {
      (cache: File, resDirs: Seq[File], outDir: File) => {
    import querease._
    import metadata._
    trait MyI18nRules extends SuffixI18nRules { this: Metadata =>
      override val i18nSuffixes = Set("_eng", "_rus")
    }
    object TestPack
      extends YamlTableDefLoader
      with FilesMdSource
      with MyI18nRules
      with TresqlJoinsParser
      with AllExpressionsFilterable
      with YamlViewDefLoader
      with Metadata {
      override def typedefFiles =
        resDirs.flatMap(recursiveListFiles).toSeq.filter(filter)
    }
    object ScalaBuilder extends ScalaClassWriter {
      override def scalaClassTraits(typeDef: XsdTypeDef) =
        if (typeDef.fields.exists(f => f.name == "id" && f.xsdType.name == "long"))
          List("DtoWithId")
        else List("Dto")
    }
    val file = outDir / "dto" / "Dtos.scala"
    val contents = ScalaBuilder.createScalaClassesString(
      List("package dto", "",
        "import querease._", ""), TestPack.typedefs, Nil)
    IO.write(file, contents)
    Seq(file) // FIXME where's my cache?
  }
}
