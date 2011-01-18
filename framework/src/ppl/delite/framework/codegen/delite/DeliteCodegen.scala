package ppl.delite.framework.codegen.delite

import generators.{DeliteGenScalaVariables, DeliteGenTaskGraph}
import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._
import ppl.delite.framework.{Config, DeliteApplication}
import collection.mutable.HashMap


/**
 * Notice that this is using Effects by default, also we are mixing in the Delite task graph code generator
 */
trait DeliteCodegen extends GenericFatCodegen {
  val IR: Expressions with FatExpressions with Effects
  import IR._

  // these are the target-specific kernel generators (e.g. scala, cuda, etc.)
  type Generator = GenericFatCodegen{val IR: DeliteCodegen.this.IR.type}
  val generators : List[Generator]

  // per kernel, used by DeliteGenTaskGraph
  var controlDeps : List[Sym[_]] = _

  // global, used by DeliteGenTaskGraph
  val kernelMutatingDeps : HashMap[Sym[_],List[Sym[_]]] = new HashMap() // from kernel to its mutating deps    
  val kernelInputDeps : HashMap[Sym[_],List[Sym[_]]] = new HashMap() // from kernel to its input deps


  def ifGenAgree[A](f: Generator => A, shallow: Boolean): A = {
    val save = generators map { _.shallow }
    generators foreach { _.shallow = shallow }
    val result = generators map f
    if (result.distinct.length != 1){
      system.error("DeliteCodegen: generators disagree")
    }
    for (i <- 0 until generators.length) {
      generators(i).shallow = save(i)
    }
    result(0)
  }


  // these are overridden for specific node types in the target generators but *not* here
  
  // DISCUSS: we could move them back into the Exp traits, then ifGenAgree would no longer be needed
  
  override def syms(e: Any): List[Sym[Any]] = ifGenAgree(_.syms(e), shallow)
  override def boundSyms(e: Any): List[Sym[Any]] = ifGenAgree(_.boundSyms(e), shallow)
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = system.error("getFreeVarNode no longer used")

  //override def buildScheduleForResult(start: Exp[_]): List[TP[_]] = ifGenAgree(_.buil) <--- maybe override for performance reasons ...

  // TODO: move to some other place? --> get rid of duplicate in embedded generators!
  override def fatten(e: TP[_]): TTP = ifGenAgree(_.fatten(e), shallow)

  // fusion stuff...
  override def unapplySimpleIndex(e: Def[Any]) = ifGenAgree(_.unapplySimpleIndex(e), shallow)
  override def unapplySimpleCollect(e: Def[Any]) = ifGenAgree(_.unapplySimpleCollect(e), shallow)



  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    val x = fresh[A]
    val y = reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    println("-- emitSource")
    availableDefs.foreach(println)


    stream.println("{\"DEG\":{\n"+
                   "\"version\" : 0.1,\n"+
                   "\"kernelpath\" : \"" + Config.build_dir  + "\",\n"+
                   "\"ops\": [")

    stream.println("{\"type\" : \"Arguments\" , \"kernelId\" : \"x0\"},")
    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))
    stream.println("{\"type\":\"EOP\"}\n]}}")


    stream.flush
  }

/*
  override def focusBlock[A](result: Exp[_])(body: => A): A = {
  }

  override def focusExactScope[A](result: Exp[_])(body: List[TP[_]] => A): A = {
    super.focusExactScope(result) { levelScope =>
      
    }
  }
*/

/*
  override def emitBlockFocused(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    println("-- block")
    availableDefs.foreach(println)
    focusExactScope(result) { levelScope =>
      println("-- exact")
      availableDefs.foreach(println)
      
      val effects = result match {
        case Def(Reify(x, effects0)) =>
          levelScope.filter(effects0 contains _.sym)
        case _ => Nil
      }
      
      for (TP(sym, rhs) <- levelScope) {
        // we only care about effects that are scheduled to be generated before us, i.e.
        // if e4: (n1, n2, e1, e2, n3), at n1 and n2 we want controlDeps to be Nil, but at
        // n3 we want controlDeps to contain e1 and e2
        controlDeps = levelScope.takeWhile(_.sym != sym) filter { effects contains _ } map { _.sym }
        emitNode(sym, rhs)
      }
    }
  }
*/

  override def emitFatBlockFocused(currentScope: List[TTP])(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    println("-- block")
    availableDefs.foreach(println)

    focusExactScopeFat(currentScope)(result) { levelScope => 
      println("-- exact")
      availableDefs.foreach(println)
      
      val effects = result match {
        case Def(Reify(x, effects0)) =>
          levelScope.filter(fb => fb.lhs.exists(effects0 contains _)) // all e whose lhs contains an effect
        case _ => Nil
      }
      
      for (TTP(syms, rhs) <- levelScope) {
        // we only care about effects that are scheduled to be generated before us, i.e.
        // if e4: (n1, n2, e1, e2, n3), at n1 and n2 we want controlDeps to be Nil, but at
        // n3 we want controlDeps to contain e1 and e2
        controlDeps = levelScope.takeWhile(_.lhs != syms) filter { effects contains _ } flatMap { _.lhs }
        emitFatNode(syms, rhs)
      }
    }
  }



  /**
   * DeliteCodegen expects there to be a single schedule across all generators, so a single task graph
   * can be generated. This implies that every generator object must compute internal dependencies (syms)
   * the same way.
   *
   * This is all because we allow individual generators to refine their dependencies, which directly impacts
   * the generated schedule. We may want to consider another organization.
   */
/*
  override def emitBlock(start: Exp[_])(implicit stream: PrintWriter): Unit = {
    if (generators.length < 1) return

    // verify our single schedule assumption
    val e1 = ifGenAgree(_.buildScheduleForResult(start), false) // deep
    val e2 = ifGenAgree(_.buildScheduleForResult(start), true) // shallow

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val effects = start match {
      case Def(Reify(x, effects0)) =>
        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        e4.filter(effects contains _)
      case _ => Nil
    }

    val save = scope
    scope = e4 ::: scope
    generators foreach { _.scope = scope }

    for (t@TP(sym, rhs) <- e4) {
      // we only care about effects that are scheduled to be generated before us, i.e.
      // if e4: (n1, n2, e1, e2, n3), at n1 and n2 we want controlDeps to be Nil, but at
      // n3 we want controlDeps to contain e1 and e2
      controlDeps = e4.take(e4.indexOf(t)) filter { effects contains _ } map { _.sym }
      emitNode(sym, rhs)
    }


    start match {
      case Def(Reify(x, effects0)) =>
        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        val actual = e4.filter(effects contains _)

        // actual must be a prefix of effects!
        assert(effects.take(actual.length) == actual,
            "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)

        val e5 = effects.drop(actual.length)

        for (TP(_, rhs) <- e5) {
          emitNode(Sym(-1), rhs)
        }
      case _ =>
    }

    generators.foreach(_.scope = save)
    scope = save
  }
*/

  /*
  def getEffectsKernel(start: Sym[_], rhs: Def[_]): List[Sym[_]] = {
    val e1 = ifGenAgree(_.buildScheduleForResult(start), false) // deep
    val params = ifGenAgree(_.syms(rhs), true)
    val e2 = params map { s => ifGenAgree(_.buildScheduleForResult(s), false) }
    val e3 = if (!e2.isEmpty) e2 reduceLeft { (a,b) => a union b } else Nil

    // e3 is missing some effect dependencies outside of the block
    // shallow might contain those? (nope)

    // we almost want a "deep on everything except this symbol" search

    val e4 = ifGenAgree(_.buildScheduleForResult(start), true) // shallow
    //val e3 = scope.drop(scope.indexOf(findDefinition(start).get)) filter { e2 contains _ }
    val e5 = e1 filterNot { d => (e3 contains d) || (e4 contains d) }

    e5 flatMap { e =>
      e.sym match {
        case Def(Reflect(x, effects)) => List(e.sym): List[Sym[_]]
        case _ => Nil
      }
    }
  }
  */


  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }

}

trait DeliteCodeGenPkg extends DeliteGenTaskGraph

trait DeliteCodeGenOverridesScala extends DeliteGenScalaVariables
