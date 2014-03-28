/**********************************************************************************************\
* Rapture Data Library                                                                         *
* Version 0.9.0                                                                                *
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

import language.{dynamics, higherKinds}

object DataCompanion {
  object Empty
}

trait DataCompanion[+Type <: DataType[Type, ParserType], ParserType[S] <: DataParser[S]] {
  
  def construct(any: Any, path: Vector[Either[Int, String]])(implicit parser: ParserType[_]): Type = constructRaw(Array(any), path)
  
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser: ParserType[_]): Type
  
  def parse[Source: ParserType](s: Source)(implicit eh: ExceptionHandler):
      eh.![Type, ParseException] = eh.wrap {
    construct(try implicitly[ParserType[Source]].parse(s).get catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    }, Vector())
  }
  
  def apply[T: Wrapper](t: T)(implicit parser: ParserType[_]): Type =
    construct(implicitly[Wrapper[T]].wrap(t), Vector())
  
  def unapply(value: Any)(implicit parser: ParserType[_]): Option[Type] =
    Some(construct(value, Vector()))

  def format(value: Option[Any], ln: Int, parser: ParserType[_], pad: String,
      brk: String): String
  
}

object DataType {
  type ByType[+T <: DataType.ByType[T]] = DataType[T, P] forSome { type P[S] <: DataParser[S] }
}

trait DataType[+T <: DataType[T, ParserType], ParserType[S] <: DataParser[S]] extends Dynamic {
  def companion: DataCompanion[T, ParserType]
  protected def root: Array[Any]
  implicit def parser: ParserType[_]
  def path: Vector[Either[Int, String]]
  protected def doNormalize(orEmpty: Boolean): Any
  def normalize = doNormalize(false)
  
  /** Navigates the JSON using the `List[String]` parameter, and returns the element at that
    * position in the tree. */
  def normalizeOrNil: Any =
    try normalize catch { case e: Exception => parser.fromArray(List()) }

  def normalizeOrEmpty: Any =
    try normalize catch { case e: Exception => parser.fromObject(Map()) }

  def format: String = companion.format(Some(root(0)), 0, parser, " ", "\n")

  def serialize: String = companion.format(Some(normalize), 0, parser, "", "")
  
  def apply(i: Int): T =
    companion.constructRaw(root, Left(i) +: path)
  
  def applyDynamic(key: String)(i: Int): T = selectDynamic(key).apply(i)
  
  override def equals(any: Any) = any match {
    case any: DataType[_, _] => root(0) == any.root(0)
    case _ => false
  }

  def as[V](implicit unwrapper: Unwrapper[V, T], eh: ExceptionHandler): eh.![V, DataGetException]
  
  override def hashCode = root(0).hashCode & "json".hashCode

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T =
    companion.constructRaw(root, Right(key) +: path)

  def extract(sp: Vector[String]): DataType[T, ParserType] =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)
  
  override def toString = format
  
}

trait MutableDataType[+T <: DataType[T, ParserType], ParserType[S] <: MutableDataParser[S]]
    extends DataType[T, ParserType] {

  def setRoot(value: Any): Unit

  def updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit = p match {
    case Vector() =>
      setRoot(newVal)
    case Left(idx) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, parser.setArrayValue(jb.normalizeOrNil, idx, newVal))
    case Right(key) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, parser.setObjectValue(jb.normalizeOrEmpty, key, newVal))
  }
  
  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion): Unit =
    updateParents(path, parser.setObjectValue(normalizeOrEmpty, key, v.value))
 
  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T: Wrapper](i: Int, v: T): Unit =
    updateParents(path, parser.setArrayValue(normalizeOrNil, i,
        implicitly[Wrapper[T]].wrap(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = updateParents(path, parser.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T: Wrapper](v: T): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) parser.fromArray(Nil) else r
    updateParents(path, parser.addArrayValue(insert, implicitly[Wrapper[T]].wrap(v)))
  }
  
}

object ForcedConversion {
  implicit def forceConversion[T: Wrapper](t: T) =
    ForcedConversion(implicitly[Wrapper[T]].wrap(t))
}
case class ForcedConversion(var value: Any)

