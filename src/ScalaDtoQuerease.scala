package org.mojoz.querease

import scala.collection.immutable.TreeMap
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ManifestFactory
import scala.util.Try

import java.util.concurrent.ConcurrentHashMap

import org.tresql.Column
import org.tresql.RowLike


trait ScalaDtoQuereaseIo extends QuereaseIo with QuereaseResolvers { self: Querease =>

  override type DTO <: Dto
  type DWI <: DTO with DtoWithId

  override def convertRow[B <: DTO](row: RowLike)(implicit mf: Manifest[B]): B =
    rowLikeToDto(row, mf)
  override def toSaveableMap[B <: DTO](dto: B): Map[String, Any] =
    dto.asInstanceOf[B{type QE = self.type}].toSaveableMap(this)
  override def keyMap[B <: DTO](dto: B) = dto match {
    case o: DtoWithId => Map("id" -> o.id)
    case _ => sys.error( // TODO use viewDef to get key-values if defined
      s"getting key map for ${dto.getClass.getName} not supported yet")
  }

  def toMap[B <: DTO](dto: B): Map[String, Any] =
    dto.asInstanceOf[B{type QE = Querease with ScalaDtoQuereaseIo} /*self.type somehow does not work*/].toMap(this)

  //org.tresql.Converter[T]
  implicit def rowLikeToDto[B <: DTO](r: RowLike, m: Manifest[B]): B =
    m.runtimeClass.newInstance.asInstanceOf[B{type QE = self.type}].fill(r)(this)
}

private[querease] object DtoReflection {
  private val cache =
    new ConcurrentHashMap[Class[_ <: Dto], Map[String, (java.lang.reflect.Method, (Manifest[_], Manifest[_ <: Dto]))]]
  def manifest(name: String, dto: Dto, clazz: Class[_]): (Manifest[_], Manifest[_ <: Dto]) =
    if (!clazz.isPrimitive)
      ManifestFactory.classType(clazz) ->
        (if (classOf[Seq[_]].isAssignableFrom(clazz) && clazz.isAssignableFrom(classOf[List[_]])) { //get child type
          childManifest(dto.getClass.getMethods.filter(_.getName == name).head.getGenericReturnType)
        } else null)
    else (clazz match {
      case java.lang.Integer.TYPE => ManifestFactory.Int
      case java.lang.Long.TYPE => ManifestFactory.Long
      case java.lang.Double.TYPE => ManifestFactory.Double
      case java.lang.Boolean.TYPE => ManifestFactory.Boolean
    }) -> null
  def childManifest(t: java.lang.reflect.Type): Manifest[_ <: Dto] = {
    val parametrisedType = t.asInstanceOf[java.lang.reflect.ParameterizedType]
    val clazz = parametrisedType.getActualTypeArguments()(0) match {
      case c: Class[_] => c
      case v: java.lang.reflect.TypeVariable[_] =>
        v.getGenericDeclaration.asInstanceOf[Class[_]]
    }
    ManifestFactory.classType(clazz)
  }
  def setters(dto: Dto): Map[String, (java.lang.reflect.Method, (Manifest[_], Manifest[_ <: Dto]))] = {
    val dtoClass = dto.getClass
    cache.getOrDefault(dtoClass, {
      val setters =
        (for (
          m <- dtoClass.getMethods
          if m.getName.endsWith("_$eq") &&
            !java.lang.reflect.Modifier.isFinal(m.getModifiers) &&
            m.getParameterTypes.length == 1
        ) yield {
          val name = m.getName.dropRight(4)
          name -> (m -> manifest(name, dto, m.getParameterTypes()(0)))
        }).toMap
      cache.putIfAbsent(dtoClass, setters)
      setters
    })
  }
}

trait Dto { self =>

  protected type QE <: QuereaseMetadata with QuereaseResolvers
  //protected type QE <: Querease with ScalaDtoQuereaseIo { type DTO >: self.type }
  protected type QDto >: Null <: Dto { type QE = self.QE }

  // populate from RowLike
  def fill(r: RowLike)(implicit qe: QE): this.type = {
    for (i <- 0 until r.columnCount) r.column(i) match {
      case Column(_, name, _) if name != null => set(name, r)
      case _ =>
    }
    this
  }

  protected def setters = DtoReflection.setters(this)
  protected def set(dbName: String, r: RowLike)(implicit qe: QE) =
    (for (s <- setters.get(dbToPropName(dbName))) yield {
      //declare local converter
      def conv[A <: QDto](r: RowLike, m: Manifest[A]): A = m.runtimeClass.newInstance().asInstanceOf[A].fill(r)
      if (s._2._2 != null) { //child result
        val m: Manifest[_ <: Dto] = s._2._2
        val childResult = r.result(dbName)
        s._1.invoke(this, childResult.list[QDto](conv, m.asInstanceOf[Manifest[QDto]]).asInstanceOf[Object])
      } else if (classOf[Dto].isAssignableFrom(s._2._1.runtimeClass)) { // single child result
        val m: Manifest[_ <: Dto] = s._2._1.asInstanceOf[Manifest[_ <: Dto]]
        val childResult = r.result(dbName)
        s._1.invoke(this, childResult.list[QDto](conv, m.asInstanceOf[Manifest[QDto]]).headOption.orNull.asInstanceOf[Object])
      } else s._1.invoke(this, r.typed(dbName)(s._2._1).asInstanceOf[Object])
      s /*return setter used*/
    }) getOrElse {
      setExtras(dbName, r)
    }
  protected def setExtras(dbName: String, r: RowLike)(implicit qe: QE): AnyRef = {
    throw new RuntimeException(s"Setter for '$dbName' not found")
  }
  protected def dbToPropName(name: String) = name // .toLowerCase
  protected def propToDbName(name: String) = name
  protected def manifest(name: String, clazz: Class[_]) =
    DtoReflection.manifest(name, this, clazz)
  protected def childManifest(t: java.lang.reflect.Type): Manifest[_ <: Dto] = {
    DtoReflection.childManifest(t)
  }

  def toUnorderedMap(implicit qe: QE): Map[String, Any] = setters.map { m =>
    val propName = m._1
    val propValue = getClass.getMethod(propName).invoke(this) match {
      case s: Seq[_] => s.map {
        case dto: Dto => dto.asInstanceOf[QDto].toMap
        case str: String => str
        case i: java.lang.Integer => i
      }
      case c: Dto => c.asInstanceOf[QDto].toMap
      case x => x
    }
    propName -> propValue
  }
  def toMap(implicit qe: QE): Map[String, Any] = qe.fieldOrderingOption(ManifestFactory.classType(getClass))
    .map(toMapWithOrdering).getOrElse(toUnorderedMap)
  def toMapWithOrdering(fieldOrdering: Ordering[String])(implicit qe: QE): Map[String, Any] =
    TreeMap[String, Any]()(fieldOrdering) ++ toUnorderedMap

  def containsField(fieldName: String) = setters.contains(fieldName)

  def toString(implicit qe: QE): String = {
    val view = qe.viewDef(ManifestFactory.classType(getClass))
    val fieldNames = view.fields.map { f => Option(f.alias) getOrElse f.name }
    toString(fieldNames)
  }

  protected def toString(fieldNames: Seq[String])(implicit qe: QE): String = {
    def isMisleading(s: String) =
      s.contains(" ") || s == "null" || s == "" || s(0).isDigit
    val kv: Seq[(String, Object)] = fieldNames.map { name =>
      getClass.getMethod(name).invoke(this) match {
        case s: Seq[_] => name ->
          ("(" + (s map {
            case s: String => "\"" + s + "\""
            case d: Dto => d.asInstanceOf[QDto].toString
            case o => o.toString
          }).mkString(", ") + ")")
        case s: String if isMisleading(s) => name -> ("\"" + s + "\"")
        case d: Dto => name -> d.asInstanceOf[QDto].toString
        case x => name -> x
      }
    }
    s"${getClass.getName}{${kv.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
  }

  protected def identifier(s: String) = s match {
    case QuereaseExpressions.IdentifierExtractor(ident, _) => ident
    case _ => ""
  }
  protected def isSavableField(field: QuereaseMetadata#FieldDef,
                               view: QuereaseMetadata#ViewDef,
                               saveToMulti: Boolean,
                               saveToTableNames: Seq[String]) =
    (!field.isExpression &&
      !field.type_.isComplexType &&
      field.table != null &&
      (!saveToMulti &&
        field.table == view.table &&
        (field.tableAlias == null || field.tableAlias == view.tableAlias)
        ||
        saveToMulti &&
          saveToTableNames.contains(field.table) // TODO alias?
        )
      ||
      field.saveTo != null
      )
  protected def isSavableChildField(field: QuereaseMetadata#FieldDef,
                                    view: QuereaseMetadata#ViewDef,
                                    saveToMulti: Boolean,
                                    saveToTableNames: Seq[String],
                                    childView: QuereaseMetadata#ViewDef): Boolean =
    true
  protected lazy val saveableValue: QE => String => PartialFunction[Any, List[(String, Any)]] = implicit qe => {
    val view = qe.viewDef(ManifestFactory.classType(getClass))
    val saveToMulti = view.saveTo != null && view.saveTo.nonEmpty
    val saveTo = if (!saveToMulti) Seq(view.table) else view.saveTo
    val saveToTableNames = saveTo.map(identifier)
    // FIXME child handling, do we rely on metadata or method list?
    def isForInsert = this match {
      case o: DtoWithId => o.id == null
      case _ => sys.error(s"isForInsert() for ${getClass.getName} not supported yet") // TODO isForInsert
    }
    def isForUpdate = this match {
      case o: DtoWithId => o.id != null
      case _ => sys.error(s"isForUpdate() for ${getClass.getName} not supported yet") // TODO isForUpdate
    }
    def isSaveableField(field: QuereaseMetadata#FieldDef) =
      isSavableField(field, view, saveToMulti, saveToTableNames)
    def isSaveableChildField(field: QuereaseMetadata#FieldDef, childView: QuereaseMetadata#ViewDef) =
      isSavableChildField(field, view, saveToMulti, saveToTableNames, childView)
    def tablesTo(v: QuereaseMetadata#ViewDef) =
      if (v.saveTo != null && v.saveTo.nonEmpty)
        v.saveTo.mkString("#")
      else if (v.saveTo == Nil)
        null
      else if (v.table != null)
        v.table
      else
        null
    def childSaveTo(field: QuereaseMetadata#FieldDef, childView: QuereaseMetadata#ViewDef) =
      Option(field).map(_.saveTo).filter(_ != null) getOrElse tablesTo(childView)
    def isChildTableField(field: QuereaseMetadata#FieldDef) =
      !field.isExpression &&
        field.type_.isComplexType &&
        qe.viewDefOption(field.type_.name).map(tablesTo).orNull != null
    def saveableEntries(f: QuereaseMetadata#FieldDef, v: Any): Seq[(String, Any)] = {
      // TODO various mixes of options and params etc?
      val name = f.name
      def alias = Option(f.alias).getOrElse(f.name)
      if (f.saveTo == null && f.resolver == null)
        Seq(name + Option(f.options).getOrElse("") -> v)
      else {
        val resolvers = qe.allResolvers(view, f)
        if (resolvers.isEmpty) {
          throw new RuntimeException(
            s"Failed to imply resolver for ${view.name}.$alias")
        }
        val fSaveTo = Option(f.saveTo) getOrElse name
        val resolvedVal =
          //if complex type encountered, call toMap since values may be used in resolvers.
          if (v != null && f.type_.isComplexType && !f.isCollection) v.asInstanceOf[QDto].toMap else v
        resolvers
          .map(r => (alias + "->") -> (fSaveTo + "=" + r)) ++ Seq(alias -> resolvedVal)
      }
    }
    propName => {
      val fieldName = propToDbName(propName)
      val propFieldDefOpt = view.fields
        .find(f => Option(f.alias).getOrElse(f.name) == fieldName)
      val childTableFieldDefOpt = propFieldDefOpt
        .filter(isChildTableField)
      val saveableValueFunc: PartialFunction[Any, List[(String, Any)]] = {
        case Nil => childTableFieldDefOpt
          .flatMap(f => qe.viewDefOption(f.type_.name)
            .map(v => v -> childSaveTo(f, v))
            .filter(_._2 != null)
            .filter(vs => isSaveableChildField(f, vs._1))
            .map(_._2)
            .map(_ + Option(f.options).getOrElse("") -> Nil))
          .orElse(Some("*" + propName -> Nil)) // prefix * to avoid clashes
          .toList
        case s: Seq[_] =>
          val options = childTableFieldDefOpt
            .map(_.options).filter(_ != null) getOrElse ""
          val f = childTableFieldDefOpt.orNull
          //objects from one list can be put into different tables
          s.asInstanceOf[Seq[QDto]] map { d =>
            qe.viewDefOption(ManifestFactory.classType(d.getClass))
              .map(v => v -> childSaveTo(f, v))
              .filter(_._2 != null)
              .filter(vs => isSaveableChildField(f, vs._1))
              .map(_._2)
              .map(_ + options -> d.toSaveableMap)
              .getOrElse("*" + propName -> d.toSaveableMap) // prefix * to avoid clashes
          } groupBy (_._1) map (t => t.copy(_2 = t._2.map(_._2))) toList
        case d if childTableFieldDefOpt.isDefined =>
          val value = Option(d).map { case x: Dto => x.asInstanceOf[QDto].toSaveableMap }.orNull
          childTableFieldDefOpt
          .filter(f => isSaveableChildField(f, qe.viewDefOption(f.type_.name).get))
          .map { f =>
            val tables = saveToTableNames.map(qe.tableMetadata.tableDef)
            val childTableName = qe.viewDefOption(f.type_.name).map(tablesTo).get
            val key = // XXX too complicated to save a child
            // TODO support multiple, multiple direction, multi-col etc. refs properly
              Option(f.saveTo)
                .getOrElse {
                  tables
                    .sorted( // sort is stable
                      Ordering.by((table: org.mojoz.metadata.TableDef.TableDefBase[_]) =>
                        if (table.name == f.table) 0 else 1))
                    .find { table => table.refs.exists(_.refTable == childTableName) }
                    .map { table => table -> table.refs.count(_.refTable == childTableName) }
                    .map {
                      case (table, 1) => // TODO multi-col?
                        table.refs.find(_.refTable == childTableName).map(_.cols.head).get
                      case (table, n) => // FIXME analyze aliases and/or joins?
                        f.name + "_id"
                    }.getOrElse(childTableName)
                }
            key + Option(f.options).getOrElse("") -> value
          }
          .orElse(Some("*" + propName -> value)) // prefix * to avoid clashes
          .toList
        case x => propFieldDefOpt
          .filter(isSaveableField)
          .map(f => saveableEntries(f, x))
          .orElse(Some(List("*" + propName -> x))) // prefix * to avoid clashes
          .toList.flatten
      }
      saveableValueFunc
    }
  }
  //creating map from object
  def toSaveableMap(implicit qe: QE): Map[String, Any] = setters.toList.flatMap { m =>
    val propName = m._1
    Try(getClass.getMethod(propName).invoke(this)).toOption.map {
      saveableValue(qe)(propName)(_)
    } getOrElse Nil
  }.groupBy { case (_, _: Seq[_]) => "s" case _ => "v" } //child objects from different lists can be put into one table
    .flatMap {
    case ("s", seq) =>
      seq.asInstanceOf[Seq[(String, Seq[_])]] groupBy (_._1) map {
        t => t.copy(_2 = t._2.flatMap(_._2))
      }
    case (_, kv) => kv
  }
}

trait DtoWithId extends Dto {
  def id: java.lang.Long
  def id_=(id: java.lang.Long): Unit
}
