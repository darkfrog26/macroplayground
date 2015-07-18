import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros._

/**
 * @author Matt Hicks <matt@outr.com>
 */
object Testing {
  /*def test(s: String) = macro impl

  def impl(c: whitebox.Context)(s: c.Tree) = {
    import c.universe._
    q"""
       new {
          class MyTest {
            def hello() = println("Hello World!")
          }
          def create() = new MyTest
       }.create()
    """
  }*/
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class typedTable[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Table = macro TableGenerator.impl[T]
}

object TableGenerator {
  def impl[T](c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Table] = {
    import c.universe._
    // TODO: how do I access the typetag for T?
//    val tpe = weakTypeOf[T]
//    val tpe = c.typeOf[T]
//    val tpe = c.typecheck(q"(??? : T)").tpe
//    val fields = tpe.decls.collectFirst {
//      case m: MethodSymbol if m.isPrimaryConstructor => m
//    }.get.paramLists.head
//    fields.foreach(println)

    def modifiedObject(objectDef: ModuleDef): c.Expr[Table] = {
      val ModuleDef(_, objectName, template) = objectDef
      // TODO: Do a better job filtering children
      val children = template.children.filter(c => c.isDef && c.productPrefix == "ValDef" && !c.toString().startsWith("private"))
      val ret = q"""
        object $objectName extends Table {
          val id = column[Int]("id")
          val name = column[String]("name")
          ..$children
        }
      """
      c.Expr[Table](ret)
    }

    annottees.map(_.tree) match {
      case (objectDecl: ModuleDef) :: _ => modifiedObject(objectDecl)
      case x => c.abort(c.enclosingPosition, s"@table can only be applied to an object, not to $x")
    }
  }
}

trait Table {
  def column[T](name: String) = new Column[T](name)
}

class Column[T](name: String)