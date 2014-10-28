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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object Extractor {
  implicit def floatExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Float,
      Data] = ext.map(_.toFloat)

  implicit def shortExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Short,
      Data] = ext.map(_.toShort)

  implicit def intExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Int, Data] =
    ext.map(_.toInt)

  implicit def longExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Long,
      Data] = ext.map(_.toLong)

  implicit def byteExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Byte,
      Data] = ext.map(_.toInt.toByte)

  implicit def anyExtractor[Data <: DataType[_, DataAst]]: Extractor[Any, Data] =
    BasicExtractor[Any, Data](_.$root.value)

  
  implicit def genSeqExtractor[T, Coll[_], Data <: DataType[Data, R] forSome { type R <:
      DataAst }](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T,
      Coll[T]], ext: Extractor[T, Data]): Extractor[Coll[T], Data] =
    BasicExtractor[Coll[T], Data]({ x => x.$ast.getArray(x.$root.value).to[List].map({v =>
        ext.construct(x.$wrap(v), x.$ast) }).to[Coll] })

  implicit def optionExtractor[T, Data <: DataType[Data, R] forSome { type R <: DataAst }]
      (implicit ext: Extractor[T, Data]): Extractor[Option[T], Data] =
    new BasicExtractor[Option[T], Data](x => try Some(x.$root.value: Any) map (v =>
        ext.construct(x.$wrap(v), x.$ast)) catch { case e: Exception => None }) {
      override def suppressErrors = true
    }

  implicit def mapExtractor[T, Data <: DataType[Data, R] forSome { type R <: DataAst }]
      (implicit ext: Extractor[T, Data]): Extractor[Map[String, T], Data] =
    BasicExtractor[Map[String, T], Data](x => x.$ast.getObject(x.$root.value) mapValues {
        v => ext.construct(x.$wrap(v), x.$ast) })
}

@implicitNotFound("cannot extract type ${T} from ${D}.")
trait Extractor[T, -D] { ext =>
  def suppressErrors = false
  def construct(any: D, ast: DataAst): T
  def map[T2](fn: T => T2) = new Extractor[T2, D] {
    def construct(any: D, ast: DataAst): T2 = fn(ext.construct(any, ast))
  }

  def orElse[TS >: T, T2 <: TS, D2 <: D](ext2: Extractor[T2, D2]): Extractor[TS, D2] =
    new Extractor[TS, D2] {
      def construct(any: D2, ast: DataAst): TS = try ext.construct(any, ast) catch {
        case e: Exception => ext2.construct(any, ast)
      }
  }
}

case class BasicExtractor[T, -D](val cast: D => T) extends Extractor[T, D] {
  def construct(d: D, ast: DataAst) = cast(d)
}
