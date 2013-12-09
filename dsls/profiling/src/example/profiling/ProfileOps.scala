package example.profiling
import scala.virtualization.lms.common.{ScalaGenEffect, Base, EffectExp}

// this is the abstract interface of our profiling methods
trait ProfileOps extends Base {
  def profile(n: Rep[Int]) = new ProfileOpsCls(n)

  // syntax
  class ProfileOpsCls(n: Rep[Int]) {
    def times(func: => Rep[Any]) = profile_body(n, func)
  }
 
  // implementation
  def profile_body(n: Rep[Int], func: => Rep[Any]): Rep[ProfileArray]
}

trait ProfileOpsExp extends ProfileOps with EffectExp {
  case class Profile(n: Exp[Int], body: Block[Any]) extends Def[ProfileArray]

  def profile_body(n: Exp[Int], func: => Exp[Any]) = {
    reflectEffect(Profile(n, reifyEffects(func)))  // create an IR node
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Profile(n, body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenProfileOps extends ScalaGenEffect {
  val IR: ProfileOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) =
    rhs match {
      // insert instrumentation code around function body
      case Profile(n, body) => 
        var str = ""
        
        str += ("val " + quote(sym) + " = {\n")
        str += ("val out = new ProfileArray(" + quote(n) + ")\n")
        str += ("var i = 0\n")
        str += ("while (i < " + quote(n) + ") {\n")
        str += ("  val start = System.currentTimeMillis()\n")
        
        println(str)
        stream.println(str)
        
        println("// Beginning Code Generation for: " + body.toString)
        
        str = ""
        
        emitBlock(body)
        
        str += ("  val end = System.currentTimeMillis()\n")
        str += ("  val duration = (end - start)/1000f \n")
        str += ("  out._data(i) = duration\n")
        str += ("  i += 1\n")
        str += ("}\n")
        str += ("out\n")
        str += ("}\n")
        
        println(str)
        stream.println(str)

      case _ => super.emitNode(sym, rhs)
    }
}

