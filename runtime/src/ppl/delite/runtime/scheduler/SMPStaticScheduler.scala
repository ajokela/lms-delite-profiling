package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:02:57 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A completely static scheduler for an SMP system
 *
 * @author Kevin J. Brown
 *
 */

final class SMPStaticScheduler extends StaticScheduler {

  private val numThreads = Config.numThreads

  private val procs = new Array[ArrayDeque[DeliteOP]](numThreads)
  for (i <- 0 until numThreads) procs(i) = new ArrayDeque[DeliteOP]

  def schedule(graph: DeliteTaskGraph) : PartialSchedule = {
    //traverse nesting & schedule sub-graphs
    //TODO: implement functionality for nested graphs
    scheduleFlat(graph)

    ensureScheduled(graph)

    //return schedule
    createPartialSchedule
  }

  private def scheduleFlat(graph: DeliteTaskGraph) {
    val opQueue = new ArrayDeque[DeliteOP]
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      scheduleOne(op, graph)
      processConsumers(op, opQueue)
    }
  }

  //NOTE: this is currently the simple scheduler from Delite 1.0
  var nextThread = 0

  private def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph) {
    op match {
      case c: OP_Control => addControl(c)
      case _ => {
        if (op.variant != null) {
          OpHelper.makeVariant(op, graph)
          scheduleFlat(op.variant)
        }
        else if (op.isDataParallel) split(op, graph)
        else cluster(op)
      }
    }
  }

  private def cluster(op: DeliteOP) {
    //look for best place to put this op (simple nearest-neighbor clustering)
    var i = 0
    var notDone = true
    val deps = op.getDependencies
    while (i < numThreads && notDone) {
      if (deps.contains(procs(i).peekLast)) {
        procs(i).add(op)
        op.scheduledResource = i
        notDone = false
        if (nextThread == i) nextThread = (nextThread + 1) % numThreads
      }
      i += 1
    }
    //else submit op to next thread in the rotation (round-robin)
    if (notDone) {
      procs(nextThread).add(op)
      op.scheduledResource = nextThread
      nextThread = (nextThread + 1) % numThreads
    }
    op.isScheduled = true
  }

  private def enqueueRoots(graph: DeliteTaskGraph, opQueue: ArrayDeque[DeliteOP]) {
    for (op <- graph.ops) {
      op.processSchedulable
      if (op.isSchedulable) opQueue.add(op)
    }
  }

  private def processConsumers(op: DeliteOP, opQueue: ArrayDeque[DeliteOP]) {
    for (c <- op.getConsumers) {
      if (!c.isSchedulable) {//if not already in opQueue (protects against same consumer appearing in list multiple times)
        c.processSchedulable
        if (c.isSchedulable) opQueue.add(c)
      }
    }
  }

  private def split(op: DeliteOP, graph: DeliteTaskGraph) {
    scheduleOne(OpHelper.expand(op, numThreads, graph), graph)
    for (i <- 0 until numThreads) {
      val chunk = OpHelper.split(op, i, numThreads, graph.kernelPath)
      procs(i).add(chunk)
      chunk.scheduledResource = i
      chunk.isScheduled = true
    }
  }

  private def addControl(op: OP_Control) {
    for (i <- 0 until numThreads) {
      val chunk = op.makeChunk(i)
      procs(i).add(chunk)
      chunk.scheduledResource = i
      chunk.isScheduled = true
    }
  }

  private def ensureScheduled(graph: DeliteTaskGraph) {
    for (op <- graph.ops) {
      if (!op.isScheduled)
        error("Graph dependencies are unsatisfiable")
    }
  }

  private def createPartialSchedule = {
    new PartialSchedule(procs)
  }

}
