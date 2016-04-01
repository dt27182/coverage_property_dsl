package CoveragePropertyDSL

import Chisel._
import scala.collection.mutable._

object Hi {
  def main(args: Array[String]) = println("Hi!")
}

object CoverProperty {
    def testFunc() = println("Testing")
}

trait CanGenFSM {
    def genFSM(): (Bool, Bool, Bool) //returned tuple contains chisel wires for activate(FSM input to activate FSM), done(FSM output indicating completion), match(FSM output indicating match found)
}

abstract class Sequence extends CanGenFSM{
    val children:ArrayBuffer[Sequence] = ArrayBuffer.empty[Sequence]
}

abstract class AtomicSequence(chiselWire: Bool) extends Sequence {
    val DUTSignal: Bool = chiselWire
}

class StartSequence(chiselWire: Bool) extends AtomicSequence(chiselWire) {
    override def genFSM(): (Bool, Bool, Bool) = {
        val active = Bool()
        val match = DUTSignal
        val done = Bool(true)

        return (active, done, match)
    }
}

class DelaySequence(chiselWire: Bool, numCycles: Int) extends AtomicSequence(chiselWire) {
    assert (numCycles > 0)
    val numDelayedCycles = numCycles

    override def genFSM(): (Bool, Bool, Bool) = {
        val active = Bool()

        val counter = Reg(UInt(1, width=32))
        when(active) {
            counter := counter + UInt(1)
        }

        val done = counter === UInt(numDelayedCycles)

        val match = DUTSignal

        return (active, done, match)
    }
}

class ConcatSequence(atomicSequences: ArrayBuffer[AtomicSequence]) extends Sequence {
    assert (atomicSequences.length > 1)
    assert (atomicSequences(0).isInstanceOf[StartSequence])
    for (atomicSequence <- atomicSequences) {
        children += atomicSequence
    }

    override def genFSM(): (Bool, Bool, Bool) = {//TODO
        val active = Bool()
        return (Bool(), Bool(), Bool())
    }
}
