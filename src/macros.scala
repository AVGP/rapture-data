/**********************************************************************************************\
* Rapture Data Library                                                                         *
* Version 1.0.5                                                                                *
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

object Macros {
 
  def extractorMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: blackbox.Context): c.Expr[Extractor[T,
      Data]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)
    val extractor = typeOf[Extractor[_, _]].typeSymbol.asType.toTypeConstructor

    val params = weakTypeOf[T].decls collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      val freshName = TermName(c.fresh("eval$"))
      val valDef = ValDef(
        Modifiers(),
        freshName,
        TypeTree(
          appliedType(
            extractor,
            List(p.returnType, weakTypeOf[Data])
          )
        ),
        c.inferImplicitValue(
          appliedType(
            extractor,
            List(p.returnType, weakTypeOf[Data])
          )
        )
      )
      
      
      val paramValue = c.Expr[Any](Apply(
        Select(
          Ident(TermName("data")),
          TermName("$accessWith")
        ),
        List(Literal(Constant(p.name.toString)),
          c.Expr[Extractor[_, _]](
            Ident(freshName)
          ).tree
        ))
      )
      
      val newArray = reify(VCell(paramValue.splice))
     
      val newDataArray = Apply(
        Apply(
          Select(
            New(
              TypeTree(weakTypeOf[Data])
            ),
            termNames.CONSTRUCTOR
          ),
          List(newArray.tree)
        ),
        List(Select(
          Ident(TermName("data")),
          TermName("$ast")
        ))
      )

      Block(List(valDef), Apply(
        Select(
          c.Expr[Extractor[_, _]](
            Ident(freshName)
          ).tree,
          TermName("construct")
        ),
        List(
          newDataArray,
          Select(
            Ident(TermName("data")),
            TermName("$ast")
          )
        )
      ))
    }

    val construction = c.Expr[T](
      Apply(
        Select(
          New(
            TypeTree(weakTypeOf[T])
          ),
          termNames.CONSTRUCTOR
        ),
        params.to[List]
      )
    )

    reify(new Extractor[T, Data] {
      def construct(data: Data, ast: DataAst): T = construction.splice
    })
  }

  def serializerMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: blackbox.Context)(ast: c.Expr[DataAst]):
      c.Expr[Serializer[T, Data]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[Serializer[_, _]].typeSymbol.asType.toTypeConstructor

    val construction = if(tpe.isCaseClass) {

      val params = weakTypeOf[T].decls collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      } map { p =>
        Apply(
          Select(
            Apply(
              Select(
                Ident(definitions.PredefModule),
                TermName("any2ArrowAssoc")
              ),
              List(
                Literal(Constant(p.name.toString))
              )
            ),
            TermName("$minus$greater")
          ),
          List(
            Apply(
              Select(
                c.inferImplicitValue(appliedType(serializer, List(p.returnType,
                    weakTypeOf[Data])), false, false),
                TermName("serialize")
              ),
              List(
                Select(
                  Ident(TermName("t")),
                  p.name
                )
              )
            )
          )
        )
      }

      c.Expr[Map[String, Any]](
        Apply(
          Select(
            Select(
              Ident(definitions.PredefModule),
              TermName("Map")
            ),
            TermName("apply")
          ),
          params.to[List]
        )
      )
    } else if(tpe.isSealed) {
      c.Expr[Map[String, Any]](
        Match(
          Ident(TermName("t")),
          tpe.knownDirectSubclasses.to[List] map { sc =>
            CaseDef(
              Bind(
                TermName("v"),
                Typed(
                  Ident(termNames.WILDCARD),
                  Ident(sc.asClass)
                )
              ),
              EmptyTree,
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(serializer, List(sc.asType.toType)), false,
                      false),
                  TermName("serialize")
                ),
                List(Ident(TermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Serializer[T, Data] {
      def serialize(t: T): Any = ast.splice.fromObject(construction.splice)
    })
  }
}
