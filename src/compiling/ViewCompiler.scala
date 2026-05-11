package org.mojoz.querease.compiling

import org.mojoz.metadata.ViewDef
import org.mojoz.querease._
import org.tresql.{MacroResourcesImpl, QueryCompiler, SimpleCache, ast}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.{Map, Seq}
import scala.jdk.CollectionConverters._

trait ViewCompiler extends QuereaseMetadata {
  this: QuereaseExpressions with QuereaseResolvers with QueryStringBuilder
    with QuereaseResolvers with FilterTransformer with ValueTransformer =>

  import QueryStringBuilder.CompilationUnit

  private val QueriesCatergory = "queries"
  private val ValidationsCategory = "validations"

  protected lazy val viewNameToQueryVariablesCompilerCache = {
    val cache = new ConcurrentHashMap[String, Seq[ast.Variable]]
    cache.putAll(viewNameToQueryVariablesCache.asJava)
    cache
  }

  override lazy val macroResources: MacroResourcesImpl =
    new MacroResourcesImpl(macrosInstance, tresqlMetadata, resourceClassLoader)

  /** All queries and dml-s from viewDef for compilation, together with group name - to test viewDef */
  def allQueryStrings(viewDef: ViewDef): Seq[CompilationUnit] = {
    if (viewDef.fields != null && viewDef.fields.nonEmpty &&
      (viewDef.table != null || viewDef.joins != null && viewDef.joins.nonEmpty))
      List(
        CompilationUnit(QueriesCatergory, viewDef.name, viewDef.db, queryStringAndParams(viewDef, Map.empty)._1)
      )
    else Nil
  } ++ validationsQueryStrings(viewDef).map(vq => CompilationUnit(ValidationsCategory, viewDef.name, viewDef.db, vq))

  protected def generateQueriesForCompilation(log: => String => Unit): Seq[CompilationUnit] = {
    val viewsToCompile =
      nameToViewDef.values.toList
      .filter(viewDef => !childViewNames.contains(viewDef.name)) // compile only top-level views
      .sortBy(_.name)
    log(s"Generating queries to be compiled for ${viewsToCompile.size} top-level views" +
        s" (${nameToViewDef.size} views total)")
    val startTime = System.currentTimeMillis
    val result = viewsToCompile.flatMap { v =>
      try allQueryStrings(v) catch { case util.control.NonFatal(ex) =>
        throw new RuntimeException(s"Failed to generate queries for ${v.name}", ex)
      }
    }
    val endTime = System.currentTimeMillis
    log(s"Query generation done in ${endTime - startTime} ms, ${result.size} queries generated")
    result
  }

  protected def compileQueries(
    category: String,
    compilationUnits: Seq[CompilationUnit],
    previouslyCompiledQueries: Set[String],
    showFailedViewQuery: Boolean,
    log: => String => Unit,
  ): Int = {
    log(s"Compiling $category - ${compilationUnits.size} total")
    val startTime = System.currentTimeMillis
    val dbToCompiler = compilationUnits.map(_.db).toSet.map { (db: String) => db -> new QueryCompiler(
      if (db == null) tresqlMetadata else tresqlMetadata.extraDbToMetadata(db),
      tresqlMetadata.extraDbToMetadata,
      macroResources,
      new SimpleCache(parserCacheSize)
    )}.toMap
    val compiledQueries = collection.mutable.Set[String](previouslyCompiledQueries.toSeq: _*)
    var compiledCount = 0
    compilationUnits.foreach { case cu @ CompilationUnit(_, viewName, db, q) =>
      if (!compiledQueries.contains(cu.queryStringWithContext)) {
        val compiler = dbToCompiler(db)
        try compiler.compile(q) catch { case util.control.NonFatal(ex) =>
          val msg = s"\nFailed to compile $viewName query: ${ex.getMessage}" +
            (if (showFailedViewQuery) s"\n$q" else "")
          throw new RuntimeException(msg, ex)
        }
        if (category == QueriesCatergory)
          viewNameToQueryVariablesCompilerCache.put(viewName, compiler.extractVariables(q))
        compiledCount += 1
        compiledQueries += cu.queryStringWithContext
      }
    }
    val endTime = System.currentTimeMillis
    val allQueries = compilationUnits.map(_.queryStringWithContext).toSet
    log(
      s"Compilation done - $category - ${endTime - startTime} ms, " +
      s"queries compiled: $compiledCount" +
      (if (compiledCount != allQueries.size) s" of ${allQueries.size}" else ""))
    compiledCount
  }

  /** Clear all caches used for query compilation */
  protected def clearCompilerCaches(): Unit = {}

  /** Compile all queries - to test viewDef. Used by sbt-mojoz plugin */
  def compileAllQueries(
    previouslyCompiledQueries: Set[String],
    showFailedViewQuery: Boolean,
    log: => String => Unit,
  ): (Set[String], Map[String, Array[Byte]]) = {
    val startTime = System.currentTimeMillis
    if (previouslyCompiledQueries.isEmpty)
      clearCompilerCaches() // clear caches if full recompile required, e.g. when table or type metadata changed
    val queriesForCompilation = generateQueriesForCompilation(log)
    val categorizedQueriesForCompilation = queriesForCompilation.groupBy(_.category).toSeq.sortBy(_._1)
    val compiledQueries = collection.mutable.Set[String](previouslyCompiledQueries.toSeq: _*)
    val compiledCount =
      categorizedQueriesForCompilation.map { case (category, compilationUnits) =>
        val count =
          compileQueries(category, compilationUnits, compiledQueries.toSet, showFailedViewQuery, log)
        compiledQueries ++= compilationUnits.map(_.queryStringWithContext)
        count
      }.sum
    val allQueries = queriesForCompilation.map(_.queryStringWithContext).toSet
    val endTime = System.currentTimeMillis
    log(
      s"View compilation done in ${endTime - startTime} ms, " +
      s"queries compiled: $compiledCount" +
      (if (compiledCount != allQueries.size) s" of ${allQueries.size}" else ""))
    (allQueries, serializedCaches)
  }
}
