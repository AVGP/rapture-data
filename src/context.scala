/**********************************************************************************************\
* Rapture JSON Library                                                                         *
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

class DataContext[+Data <: DataType[Data, DataAst], -AstType <: DataAst](companion: DataCompanion[Data, AstType], sc: StringContext, parser: Parser[String, AstType]) {

  def apply(exprs: ForcedConversion*)(implicit eh: ExceptionHandler): eh.![Data, ParseException] =
    eh.wrap {
      val sb = new StringBuilder
      val textParts = sc.parts.iterator
      val expressions = exprs.iterator
      sb.append(textParts.next())
      while(textParts.hasNext) {
        sb.append(companion.constructRaw(VCell(expressions.next.value), Vector())(parser.ast).toString)
        sb.append(textParts.next)
      }
      companion.constructRaw(VCell(parser.parse(sb.toString).get), Vector())(parser.ast)
    }

  def unapplySeq[D <: DataType[D, DataAst]](data: D): Option[Seq[DataType[D, DataAst]]] = try {
    val placeholder = Utils.uniqueNonSubstring(sc.parts.mkString)
    val PlaceholderNumber = (placeholder+"([0-9]+)"+placeholder).r
    val next = new Counter(0)
    val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${next()}${placeholder}" """ + _)
    val paths: Array[Vector[String]] =
      Array.fill[Vector[String]](sc.parts.length - 1)(Vector())
    
    def extract(any: Any, path: Vector[String]): Unit = {
      if(parser.ast.isScalar(any)) {
        if(data.extract(path).as[Any](?, raw) !=
            parser.ast.getScalar(any)) throw new Exception("Value doesn't match")
      } else if(parser.ast.isObject(any)) {
        parser.ast.getObject(any) foreach { case (k, v) =>
          if(parser.ast.isString(v)) parser.ast.getString(v) match {
            case Some(PlaceholderNumber(n)) =>
              paths(n.toInt) = path :+ k
            case _ => extract(v, path :+ k)
          } else extract(v, path :+ k)
        }
      } else throw new Exception("Can't match on arrays.")
    }

    extract(parser.parse(txt).get, Vector())

    val extracts = paths.map(data.extract)
    if(extracts.exists(_.$root.value == null)) None
    else Some(extracts)
  } catch { case e: Exception => None }
}
