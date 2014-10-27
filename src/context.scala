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

object patternMatching {
  implicit val exactArrays = new ArrayMatching { def checkLengths = true }
  implicit val exactObjects = new ObjectMatching { def checkSizes = true }
  implicit val exact = new ObjectMatching with ArrayMatching {
    def checkSizes = true
    def checkLengths = true
  }
}

object ArrayMatching {
  implicit val ignore = new ArrayMatching { def checkLengths = false }
}

object ObjectMatching {
  implicit val ignore = new ObjectMatching { def checkSizes = false }
}

trait ArrayMatching { def checkLengths: Boolean }
trait ObjectMatching { def checkSizes: Boolean }

class DataContext[+Data <: DataType[Data, DataAst], -AstType <: DataAst]
    (companion: DataCompanion[Data, AstType], sc: StringContext,
    parser: Parser[String, AstType]) {

  def apply(exprs: ForcedConversion[Data]*)(implicit mode: Mode[ParseMethods]):
      mode.Wrap[Data, ParseException] =
    mode wrap {
      val sb = new StringBuilder
      val textParts = sc.parts.iterator
      val expressions = exprs.iterator
      sb.append(textParts.next())
      while(textParts.hasNext) {
        sb.append(companion.construct(VCell(expressions.next.value),
            Vector())(parser.ast).toString)
        sb.append(textParts.next)
      }
      companion.construct(VCell(parser.parse(sb.toString).get), Vector())(parser.ast)
    }

  def unapplySeq[D <: DataType[D, DataAst]](data: D)(implicit arrayMatching: ArrayMatching,
      objectMatching: ObjectMatching): Option[Seq[DataType[D, DataAst]]] = try {
    val placeholder = Utils.uniqueNonSubstring(sc.parts.mkString)
    val PlaceholderNumber = (placeholder+"([0-9]+)"+placeholder).r
    val next = new Counter(0)
    val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${next()}${placeholder}" """ + _)
    
    val paths: Array[Vector[Either[Int, String]]] =
      Array.fill[Vector[Either[Int, String]]](sc.parts.length - 1)(Vector())

    val arrayLengths = new collection.mutable.HashMap[Vector[Either[Int, String]], Int]
    val objectSizes = new collection.mutable.HashMap[Vector[Either[Int, String]], Int]
    
    def extract(any: Any, path: Vector[Either[Int, String]]): Unit = {
      if(parser.ast.isScalar(any)) {
        if(data.$extract(path).as[Any] != any) throw new Exception("Value doesn't match")
      } else if(parser.ast.isObject(any)) {
        val obj = parser.ast.getObject(any)
        objectSizes(path) = obj.size
        obj foreach { case (k, v) =>
          if(parser.ast.isString(v)) parser.ast.getString(v) match {
            case PlaceholderNumber(n) =>
              paths(n.toInt) = path :+ Right(k)
            case _ => extract(v, path :+ Right(k))
          } else extract(v, path :+ Right(k))
        }
      } else if(parser.ast.isArray(any)) {
        val array = parser.ast.getArray(any)
        if(arrayMatching.checkLengths) arrayLengths(path) = array.length
        array.zipWithIndex foreach { case (e, i) =>
          if(parser.ast.isString(e)) parser.ast.getString(e) match {
            case PlaceholderNumber(n) =>
              paths(n.toInt) = path :+ Left(i)
            case _ => extract(e, path :+ Left(i))
          } else extract(e, path :+ Left(i))
        }
      } else throw new Exception("Value doesn't match")
    }

    extract(parser.parse(txt).get, Vector())

    val extracts = paths.map(data.$extract(_))
    extracts.map(_.$normalize)
    val matchedArrayLengths = arrayLengths.forall { case (p, len) =>
      parser.ast.getArray(data.$extract(p).$normalize).length == len
    }
    
    val matchedObjectSizes = objectSizes.forall { case (p, s) =>
      if(objectMatching.checkSizes) parser.ast.getObject(data.$extract(p).$normalize).size == s
      else parser.ast.getObject(data.$extract(p).$normalize).size >= 0
    }

    if(extracts.exists(_.$root.value == null) || !matchedArrayLengths || !matchedObjectSizes) None
    else Some(extracts)
  } catch { case e: Exception => None }
}
