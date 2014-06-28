/**********************************************************************************************\
* Rapture Data Library                                                                         *
* Version 0.10.0                                                                               *
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

  def unapplySeq[D <: DataType[D, DataAst]](data: D): Option[Seq[DataType[D, DataAst]]] = try {
    val placeholder = Utils.uniqueNonSubstring(sc.parts.mkString)
    val PlaceholderNumber = (placeholder+"([0-9]+)"+placeholder).r
    val next = new Counter(0)
    val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${next()}${placeholder}" """ + _)
    
    println("txt = "+txt)
    println("parser.parse(txt) = "+parser.parse(txt))

    val paths: Array[Vector[String]] =
      Array.fill[Vector[String]](sc.parts.length - 1)(Vector())
    
    def extract(any: Any, path: Vector[String]): Unit = {
      println("Extracting "+any+" at "+path)
      if(parser.ast.isScalar(any)) {
        println("isScalar")
        println("data.extract "+data.extract(path).as[Any](?, raw))
        println("ast.getScalar = "+parser.ast.getScalar(any))
        if(data.extract(path).as[Any](?, raw) !=
            parser.ast.getScalar(any)) throw new Exception("Value doesn't match")
      } else if(parser.ast.isObject(any)) {
        println("isObject")
        parser.ast.getObject(any) foreach { case (k, v) =>
          println("getting object: "+v+": "+v.getClass)
          if(parser.ast.isString(v)) { println("isString"); parser.ast.getString(v) match {
            case PlaceholderNumber(n) =>
              println("matched "+n)
              paths(n.toInt) = path :+ k
            case _ => extract(v, path :+ k)
          } } else extract(v, path :+ k)
        }
      } else throw new Exception("Can't match on arrays.")
    }

    extract(parser.parse(txt).get, Vector())

    println("paths = "+paths)

    val extracts = paths.map(data.extract)
    println("extracts = "+extracts)
    if(extracts.exists(_.$root.value == null)) None
    else Some(extracts)
  } catch { case e: Exception => None }
}
