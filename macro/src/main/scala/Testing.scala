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
class table[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TableGenerator.impl
}

object TableGenerator {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    println("impl")

    def modifiedObject(objectDef: ModuleDef): c.Expr[Any] = {
      println("Modified!")
      val ModuleDef(_, objectName, template) = objectDef
      val ret = q"""
object $objectName {
  def test() = println("Wahoo!")
  def hello() = println("Hello")
}
      """
      c.Expr[Any](ret)
    }

    annottees.map(_.tree) match {
      case (objectDecl: ModuleDef) :: _ => modifiedObject(objectDecl)
      case x => c.abort(c.enclosingPosition, s"@table can only be applied to an object, not to $x")
    }
  }
}

trait Helloable {
  def hello(): Unit
}