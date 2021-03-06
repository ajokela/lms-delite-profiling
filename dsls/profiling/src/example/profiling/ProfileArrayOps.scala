package example.profiling
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{NumericOpsExp, FractionalOpsExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteCollectionOpsExp,DeliteOpsExp}
import ppl.delite.framework.datastructures.DeliteArray


trait ProfileArrayOps extends Base {
  // a simple way of enumerating choices in our syntax
  class Reporter
  object average extends Reporter
  object median extends Reporter

  // add report and length methods to Rep[ProfileArray]
  def infix_report(x: Rep[ProfileArray], y: Reporter) = profile_report(x, y)
  def infix_length(x: Rep[ProfileArray]) = profile_length(x)

  // implementation
  def profile_report(x: Rep[ProfileArray], y: Reporter): Rep[Double]
  def profile_length(x: Rep[ProfileArray]): Rep[Int]
}

trait ProfileArrayOpsExp extends ProfileArrayOps with NumericOpsExp
  with FractionalOpsExp with DeliteCollectionOpsExp with DeliteOpsExp {

  // a Delite parallel operation! was it really that easy?
  case class ReportSum(in: Exp[ProfileArray])
    extends DeliteOpReduce[Double] {
      val zero = unit(0.0)
      val size = copyTransformedOrElse(_.size)(in.length)
      def func = (a,b) => a + b
  }

  // median is a little trickier, let's just be sequential
  case class ReportMedian(in: Exp[ProfileArray]) extends Def[Double]

  // length, apply, update need to reference the underlying data structure
  case class ProfileLength(in: Exp[ProfileArray]) extends Def[Int]
  case class ProfileApply(in: Exp[ProfileArray], n: Exp[Int]) extends Def[Double]
  case class ProfileUpdate(in: Exp[ProfileArray], n: Exp[Int], y: Exp[Double])
    extends Def[Unit]

  /////////////////////
  // delite collection

  def isProfileArray[A](x: Exp[DeliteArray[A]]) = x.isInstanceOf[Exp[ProfileArray]]
  def asProfileArray[A](x: Exp[DeliteArray[A]]) = x.asInstanceOf[Exp[ProfileArray]]

  override def darray_length[A:Manifest](x: Exp[DeliteArray[A]])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) asProfileArray(x).length
    else super.darray_length(x)
  }

  override def darray_apply[A:Manifest](x: Exp[DeliteArray[A]], n: Exp[Int])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) (profile_apply(asProfileArray(x),n)).asInstanceOf[Exp[A]]
    else super.darray_apply(x,n)
  }

  override def darray_update[A:Manifest](x: Exp[DeliteArray[A]], n: Exp[Int], y: Exp[A])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) profile_update(asProfileArray(x),n,y.asInstanceOf[Exp[Double]])
    else super.darray_update(x,n,y)
  }

  def profile_report(x: Exp[ProfileArray], y: Reporter) = y match {
    case this.average => ReportSum(x) / x.length   // inline
    case this.median => ReportMedian(x)
    case _ => throw new IllegalArgumentException("unknown report type")
  }
  def profile_length(x: Exp[ProfileArray]) = ProfileLength(x)
  def profile_apply(x: Exp[ProfileArray], n: Exp[Int]): Exp[Double]
    = ProfileApply(x,n)
  def profile_update(x: Exp[ProfileArray], n: Exp[Int], y: Exp[Double])
    = ProfileUpdate(x,n,y)
}

trait ScalaGenProfileArrayOps extends ScalaGenBase {
  val IR: ProfileArrayOpsExp
  import IR._
  
  override def emitValDef(sym: Sym[Any], rhs: String) {
    val v = "val " + quote(sym) + " = " + rhs
    println(v)
    stream.println(v)
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) =

    rhs match {
      case ReportMedian(x) =>
        val a = quote(x)
        val size = a + "._numMeasurements"
        var str = ""
        
        str += "val " + quote(sym) + " = {\n"
        str += "val d = new Array[Double]("+size+")\n"
        str += "System.arraycopy("+a+"._data, 0, d, 0, "+size+")\n"
        str += "scala.util.Sorting.quickSort(d)\n"
        str += "d(Math.ceil("+size+"/2).asInstanceOf[Int])\n"
        str += "}\n"
        
        println(str)
        
        stream.println(str)
        
      case ProfileLength(x) => 
        emitValDef(sym, quote(x) + "._numMeasurements")
        
      case ProfileApply(x,n) => 
        emitValDef(sym, quote(x) + "._data(" + quote(n) + ")")
      case ProfileUpdate(x,n,y) => 
        emitValDef(sym, quote(x) + "._data(" + quote(n) + ") = " + quote(y))
      case _ => 
        println("/***\nGenerating Node: " + quote(sym) + " = " + rhs + "\n*/")
        super.emitNode(sym, rhs)
    }
}

