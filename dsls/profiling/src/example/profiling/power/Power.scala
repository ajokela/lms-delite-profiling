package example.profiling.power
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures.DeliteArray
import codegen.delite.overrides._
import reflect.{Manifest, SourceContext}
import codegen.scala.TargetScala
import java.io.File




trait Base {
  type Rep[+T]
}

trait BaseStr extends Base {
  type Rep[+T] = String
}


trait Arith extends Base {
  implicit def unit(x: Double): Rep[Double]
  def infix_+(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def infix_*(x: Rep[Double], y: Rep[Double]): Rep[Double]
}

trait ArithStr extends Arith with BaseStr {
  implicit def unit(x: Double) = x.toString
  def infix_+(x: String, y: String) = "(%s+%s)".format(x,y)
  def infix_*(x: String, y: String) = "(%s*%s)".format(x,y)
}

trait Expressions {
  // expressions (atomic)
  abstract class Exp[+T]
  case class Const[T](x: T) extends Exp[T]
  case class Sym[T](n: Int) extends Exp[T]
  def fresh[T]: Sym[T]
  // definitions (composite, subclasses provided by other traits)

  abstract class Def[T]
  def findDefinition[T](s: Sym[T]): Option[Def[T]]
  def findDefinition[T](d: Def[T]): Option[Sym[T]]
  def findOrCreateDefinition[T](d: Def[T]): Sym[T]
  // bind definitions to symbols automatically
  implicit def toAtom[T](d: Def[T]): Exp[T] = findOrCreateDefinition(d)
  // pattern match on definition of a given symbol
  object Def {
    def unapply[T](s: Sym[T]): Option[Def[T]] = findDefinition(s)
  }
}

trait BaseExp extends Base with Expressions {
  type Rep[+T] = Exp[T]
}

trait ArithExp extends Arith with BaseExp {

  implicit def unit(x: Double) = Const(x)
  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  def infix_+(x: Exp[Double], y: Exp[Double]) = Plus(x, y)
  def infix_*(x: Exp[Double], y: Exp[Double]) = Times(x, y)
  
}

trait ArithExpOpt extends ArithExp {
  override def infix_*(x:Exp[Double],y:Exp[Double]) = (x,y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case (x, Const(1)) => x
    case (Const(1), y) => x
    case _ => super.infix_*(x, y)
  }
}

trait PowerB { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0
    else if ((x&1) == 0) { val y = power(b, x/2); y * y }
    else b * power(b, x-1)
}

trait PowerA { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0 else b * power(b, x-1)
}


trait Compile extends Base {
  def compile[A,B](f: Rep[A] => Rep[B]): A=>B
}

trait ScalaGenBase extends BaseExp {
  // def buildSchedule(Exp[_])): List[(Sym[_], Def[_])] = ...
  def emitNode(sym: Sym[_], node: Def[_]): Unit =
    throw new Exception("node " + node + " not supported")
}

trait ScalaGenArith extends ScalaGenBase with ArithExpOpt {
  override def emitNode(sym: Sym[_], node: Def[_]): Unit = node match {
    case Plus(a,b) => 
      
      val str = "val %s = %a + %b".format(quote(sym),a,b)
      println(str)
      
    case Times(a,b) => 
      
      val str = "val %s = %a * %b".format(quote(sym),a,b)
      println(str)
      
    case _ =>  throw new Exception("node " + node + " not supported")
  }
}

trait CompileScala extends Compile with ScalaGenBase {
  def compile[A,B](f: Exp[A] => Def[B]) = {
    val x = fresh[A]
    val y = f(x)
    // emit header  
    //for ((sym, node) <- buildSchedule(y))
    
    emitNode(x, y)
    
    // emit footer
    // invoke compiler
    // load generated class file
    // instantiate object of that class
  }
}

trait PowerA2 extends PowerA with ArithExpOpt with ScalaGenArith {
  //val p4 = compile { x: Rep[Double] =>
    power(fresh[Double] * fresh[Double], 4)
  //}
  // use compiled function p4 ...
}
