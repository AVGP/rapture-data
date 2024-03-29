/**********************************************************************************************\
* Rapture Data Library                                                                         *
* Version 1.0.6                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2014 Jon Pretty, Propensive Ltd.                                              *
*                                                                                              *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file    *
* except in compliance with the License. You may obtain a copy of the License at               *
*                                                                                              *
*   http://www.apache.org/licenses/LICENSE-2.0                                                 *
*                                                                                              *
* Unless required by applicable law or agreed to in writing, software distributed under the    *
* License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    *
* either express or implied. See the License for the specific language governing permissions   *
* and limitations under the License.                                                           *
\**********************************************************************************************/
package rapture.data

import rapture.core._

import scala.util.Try

import language.dynamics
import language.higherKinds
import language.existentials

@implicitNotFound("Cannot find an implicit Formatter for ${AstType} data.")
trait Formatter[-AstType <: DataAst] {
  type Out
  def format(any: Any): Out
}

object DataCompanion { object Empty }

trait DataCompanion[+Type <: DataType[Type, DataAst], -AstType <: DataAst] {

  def empty(implicit ast: AstType) =
    construct(VCell(ast.fromObject(Map())), Vector())

  def construct(any: VCell, path: Vector[Either[Int, String]])(implicit ast: AstType): Type

  def parse[Source](s: Source)(implicit mode: Mode[ParseMethods],
      parser: Parser[Source, AstType]): mode.Wrap[Type, ParseException] = mode wrap {
    construct(try VCell(parser.parse(s).get) catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    }, Vector())(parser.ast)
  }

  def apply[T](t: T)(implicit ast: AstType, ser: Serializer[T, Type]): Type =
    construct(VCell(ser.serialize(t)), Vector())

  def unapply(value: Any)(implicit ast: AstType): Option[Type] =
    Some(construct(VCell(value), Vector()))

  def format[T <: DataType[T, AstType]](data: T)(implicit f: Formatter[_ <: AstType]): f.Out =
    f.format(data.$normalize)

}

case class DPath(path: List[Either[Int, String]]) extends Dynamic {
  def selectDynamic(v: String) = DPath(Right(v) :: path)
  def applyDynamic(v: String)(i: Int) = DPath(Left(i) :: Right(v) :: path)
  def apply(i: Int) = DPath(Left(i) :: path)
}

case class VCell(var value: Any)

trait RaptureDataMethods extends ModeGroup
trait ExtractionMethods extends RaptureDataMethods
trait ParseMethods extends RaptureDataMethods

trait DynamicData[+T <: DynamicData[T, AstType], +AstType <: DataAst] extends Dynamic {

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T = $deref(Right(key) +: $path)

  def applyDynamic(key: String)(i: Int = 0): T = $deref(Left(i) +: Right(key) +: $path)

  def $deref($path: Vector[Either[Int, String]]): T
  def $path: Vector[Either[Int, String]]

}

trait DataType[+T <: DataType[T, AstType], +AstType <: DataAst] {
  val $root: VCell
  implicit def $ast: AstType
  def $path: Vector[Either[Int, String]]
  def $normalize: Any = doNormalize(false)
  def $accessInnerMap(k: String): Any = $ast.dereferenceObject($root.value, k)
  def $accessInnerMapSafe(k: String): Any =
    try $accessInnerMap(k) catch { case e: Exception => DataTypes.Undefined }
  def $wrap(any: Any, $path: Vector[Either[Int, String]] = Vector()): T
  def $deref($path: Vector[Either[Int, String]] = Vector()): T
  def $accessWith[V](k: String, ext: Extractor[V, T]): Any =
    if(ext.suppressErrors) $accessInnerMapSafe(k) else $accessInnerMap(k)
  def $extract($path: Vector[Either[Int, String]]): T

  protected def doNormalize(orEmpty: Boolean): Any =
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ e) =>
        fn(({
          if(e.bimap(x => $ast.isArray(j), x => $ast.isObject(j))) {
            try e.bimap($ast.dereferenceArray(j, _), $ast.dereferenceObject(j, _)) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, $path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException($path.drop(t.length))
            }
          } else throw TypeMismatchException(
            if($ast.isArray(j)) DataTypes.Array else DataTypes.Object,
                e.bimap(l => DataTypes.Array, r => DataTypes.Object),
            $path.drop(t.length)
          )
        }, t))
    } } ($root.value -> $path)

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def as[S](implicit ext: Extractor[S, T], mode: Mode[ExtractionMethods]):
      mode.Wrap[S, SingleDataGetException] = mode wrap {
    try ext.construct($wrap($normalize), $ast) catch {
      case TypeMismatchException(f, e, _) => throw TypeMismatchException(f, e, $path)
      case e: MissingValueException => throw e
    }
  }

  def apply(i: Int = 0): T = $deref(Left(i) +: $path)

  override def equals(any: Any) = try { any match {
    case any: DataType[_, _] => $normalize == any.$normalize
    case _ => false
  } } catch { case e: Exception => false }

  override def hashCode = $root.value.hashCode & "json".hashCode

  def ++[S <: DataType[S, Rep] forSome { type Rep }](b: S): T = {
    def merge(a: Any, b: Any): Any = {
      if($ast.isObject(b)) {
        if($ast.isObject(a)) {
          $ast.fromObject($ast.getKeys(b).foldLeft($ast.getObject(a)) { case (as, k) =>
            as + (k -> {
              if(as contains k) merge(as(k), $ast.dereferenceObject(b, k))
              else $ast.dereferenceObject(b, k)
            })
          })
        } else b
      } else if($ast.isArray(b)) {
        if($ast.isArray(a)) $ast.fromArray($ast.getArray(a) ++ $ast.getArray(b))
        else b
      } else b
    }
    $wrap(merge($normalize, b.$root.value), Vector())
  }

  def +(pv: (DPath => DPath, ForcedConversion[T])): T = if(pv._2.nothing) $wrap($normalize) else {
    def add(path: List[Either[Int, String]], v: Any): Any = path match {
      case Nil => v
      case Right(next) :: list => $ast.fromObject(Map(next -> add(list, v)))
      case Left(next) :: list => ??? // FIXME: Implement this!
    }
    this ++ $wrap(add(pv._1(DPath(Nil)).path.reverse, pv._2.value), Vector())
  }
}

trait MutableDataType[+T <: DataType[T, AstType], AstType <: MutableDataAst]
    extends DataType[T, AstType] {

  def $updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit =
    p match {
      case Vector() =>
        $root.value = newVal
      case Left(idx) +: init =>
        val jb = $deref(init)
        val newJb = $ast.setArrayValue(Try(jb.$normalize).getOrElse($ast.fromArray(Nil)), idx, newVal)
        if(jb match {
          case jb: AnyRef => newJb match {
            case newJb: AnyRef => jb ne newJb
            case _ => false
          }
          case jb => jb == newJb
        }) $updateParents(init, newJb)
      case Right(key) +: init =>
        val jb = $deref(init)
        val newJb = $ast.setObjectValue(Try(jb.$normalize).getOrElse($ast.fromObject(Map())), key, newVal)
        if(jb match {
          case jb: AnyRef => newJb match {
            case newJb: AnyRef => jb ne newJb
            case _ => false
          }
          case jb => jb == newJb
        }) $updateParents(init, newJb)
    }

  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion[T]): Unit =
    if(!v.nothing) $updateParents($path,
        $ast.setObjectValue(Try($normalize).getOrElse($ast.fromObject(Map())), key, v.value))

  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T2](i: Int, v: T2)(implicit ser: Serializer[T2, T]): Unit =
    $updateParents($path, $ast.setArrayValue(Try($normalize).getOrElse($ast.fromArray(Nil)), i,
        ser.serialize(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = $updateParents($path, $ast.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T2](v: T2)(implicit ser: Serializer[T2, T]): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) $ast.fromArray(Nil) else r
    $updateParents($path, $ast.addArrayValue(insert, ser.serialize(v)))
  }
}

object ForcedConversion extends LowPriorityForcedConversion {
  implicit def forceOptConversion[T, D](opt: Option[T])(implicit ser: Serializer[T, D]) =
    opt.map(t => ForcedConversion[D](ser.serialize(t), false)) getOrElse
        ForcedConversion[D](null, true)
}

trait LowPriorityForcedConversion {
  implicit def forceConversion[T, D](t: T)(implicit ser: Serializer[T, D]) =
    ForcedConversion[D](ser.serialize(t), false)
}

case class ForcedConversion[-D](value: Any, nothing: Boolean)

case class ParseException(source: String, line: Option[Int] = None, column: Option[Int] = None)
    extends Exception { override def toString = "Failed to parse source" }
