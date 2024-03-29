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

object Macros {

  val emittedWarnings = new collection.mutable.HashSet[String]

  def extractorMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: Context): c.Expr[Extractor[T,
      Data]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)
    val extractor = typeOf[Extractor[_, _]].typeSymbol.asType.toTypeConstructor

    val implicitSearchFailures = collection.mutable.ListMap[String, List[String]]().withDefault(_ => Nil)
    val params = weakTypeOf[T].declarations collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      val freshName = newTermName(c.fresh("eval$"))
      val valDef = ValDef(
        Modifiers(),
        freshName,
        TypeTree(
          appliedType(
            extractor,
            List(p.returnType, weakTypeOf[Data])
          )
        ),
        try c.inferImplicitValue(
          appliedType(
            extractor,
            List(p.returnType, weakTypeOf[Data])
          ), false, false
        ) catch { case e: Exception =>
          implicitSearchFailures(p.returnType.toString) ::= p.name.toString
          null
        }
      )
      
      val paramValue = c.Expr[Any](Apply(
        Select(
          Ident(newTermName("data")),
          newTermName("$accessWith")
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
            nme.CONSTRUCTOR
          ),
          List(newArray.tree)
        ),
        List(Select(
          Ident(newTermName("data")),
          newTermName("$ast")
        ))
      )

      Block(List(valDef), Apply(
        Select(
          c.Expr[Extractor[_, _]](
            Ident(freshName)
          ).tree,
          newTermName("construct")
        ),
        List(
          newDataArray,
          Select(
            Ident(newTermName("data")),
            newTermName("$ast")
          )
        )
      ))
    }

    if(!implicitSearchFailures.isEmpty) {
      val paramStrings = implicitSearchFailures map {
        case (t, p1 :: Nil) => s"parameter `$p1' of type $t"
        case (t, p1 :: ps) => s"parameters ${ps.map(v => s"`$v'").mkString(", ")} and `$p1' of type $t"
      }
      val errorString = paramStrings.to[List].reverse match {
        case t1 :: Nil => t1
        case t1 :: ts => s"${ts.mkString(", ")} and $t1"
      }
      val plural = if(implicitSearchFailures.flatMap(_._2).size > 1) s"${weakTypeOf[Data].typeSymbol.name} extractors" else s"a ${weakTypeOf[Data].typeSymbol.name} extractor"
      
      val err = s"Could not generate a ${weakTypeOf[Data].typeSymbol.name} extractor for case class ${weakTypeOf[T].typeSymbol.name} because $plural for $errorString could not be found"
      if(!emittedWarnings.contains(err)) {
        emittedWarnings += err
        c.info(NoPosition, err, true)
      }
    }

    require(implicitSearchFailures.isEmpty)
      
    val construction = c.Expr[T](
      Apply(
        Select(
          New(
            TypeTree(weakTypeOf[T])
          ),
          nme.CONSTRUCTOR
        ),
        params.to[List]
      )
    )

    reify(new Extractor[T, Data] {
      def construct(data: Data, ast: DataAst): T = construction.splice
    })
  }

  def serializerMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: Context)(ast: c.Expr[DataAst]):
      c.Expr[Serializer[T, Data]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[Serializer[_, _]].typeSymbol.asType.toTypeConstructor

    val construction = if(tpe.isCaseClass) {

      val params = weakTypeOf[T].declarations collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      } map { p =>
        Apply(
          Select(
            Apply(
              Select(
                Ident(definitions.PredefModule),
                newTermName("any2ArrowAssoc")
              ),
              List(
                Literal(Constant(p.name.toString))
              )
            ),
            newTermName("$minus$greater")
          ),
          List(
            Apply(
              Select(
                c.inferImplicitValue(appliedType(serializer, List(p.returnType,
                    weakTypeOf[Data])), false, false),
                newTermName("serialize")
              ),
              List(
                Select(
                  Ident(newTermName("t")),
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
              newTermName("Map")
            ),
            newTermName("apply")
          ),
          params.to[List]
        )
      )
    } else if(tpe.isSealed) {
      c.Expr[Map[String, Any]](
        Match(
          Ident(newTermName("t")),
          tpe.knownDirectSubclasses.to[List] map { sc =>
            CaseDef(
              Bind(
                newTermName("v"),
                Typed(
                  Ident(nme.WILDCARD),
                  Ident(sc.asClass)
                )
              ),
              EmptyTree,
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(serializer, List(sc.asType.toType)), false,
                      false),
                  newTermName("serialize")
                ),
                List(Ident(newTermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Serializer[T, Data] {
      def serialize(t: T): Any = ast.splice.fromObject(construction.splice.filterNot { v =>
        ast.splice.isNull(v._2)
      })
    })
  }
}
