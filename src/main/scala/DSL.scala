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
    def genFSM(): (Bool, Bool, Bool) //returned tuple contains chisel wires for activate(FSM input to activate FSM), done(FSM output indicating completion), occurred(FSM output indicating that the sequence occurred)
}

abstract class Sequence extends CanGenFSM{
    val children:ArrayBuffer[Sequence] = new ArrayBuffer[Sequence]()

    def genChildFSMs(): (ArrayBuffer[Bool], ArrayBuffer[Bool], ArrayBuffer[Bool]) = {
        val childFSMActiveWires: ArrayBuffer[Bool] = new ArrayBuffer[Bool]()
        val childFSMDoneWires: ArrayBuffer[Bool] = new ArrayBuffer[Bool]()
        val childFSMOccurredWires: ArrayBuffer[Bool] = new ArrayBuffer[Bool]()
        for(child <- children) {
            val (active, done, occurred) = child.genFSM()
            childFSMActiveWires += active
            childFSMDoneWires += done
            childFSMOccurredWires += occurred
        }
        return (childFSMActiveWires, childFSMDoneWires, childFSMOccurredWires)
    }
}

abstract class AtomicSequence(chiselWire: Bool) extends Sequence {
    val DUTSignal: Bool = chiselWire
}

class StartSequence(chiselWire: Bool) extends AtomicSequence(chiselWire) {
    override def genFSM(): (Bool, Bool, Bool) = {
        val active = Bool()
        val occurred = Bool()
        occurred := DUTSignal
        val done = Bool(true)

        return (active, done, occurred)
    }
}

class DelaySequence(chiselWire: Bool, numCycles: Int) extends AtomicSequence(chiselWire) {
    assert (numCycles > 0)
    val numDelayedCycles = numCycles

    override def genFSM(): (Bool, Bool, Bool) = {
        //FSM input signal
        val active = Bool()

        //FSM state reset and update
        val done = Bool() 
        val counter = Reg(UInt(1, width=32))
        when(active) {
            when(!done) {
                counter := counter + UInt(1)
            }.otherwise {
                counter := UInt(1)
            }

        }

        //FSM output assignments
        done := counter === UInt(numDelayedCycles)
        val occurred = Bool()
        occurred := DUTSignal

        return (active, done, occurred)
    }
}

class ConcatSequence(atomicSequences: ArrayBuffer[AtomicSequence]) extends Sequence {
    assert (atomicSequences.length > 1)
    assert (atomicSequences(0).isInstanceOf[StartSequence])
    for (atomicSequence <- atomicSequences) {
        children += atomicSequence
    }

    override def genFSM(): (Bool, Bool, Bool) = {
        val (childFSMActiveWires, childFSMDoneWires, childFSMOccurredWires) = genChildFSMs()
        
        //FSM input signal
        val active = Bool()

        //FSM output signals
        val done = Bool()
        val occurred = Bool()

        //FSM state reset and update
        val currentState = Reg(UInt(0, width = 32))
        val nextState = UInt(width = 32)
        currentState := nextState

        //FSM nextState and output logic
        nextState := currentState
        done := Bool(false)
        occurred := Bool(false)
        when (active) {
            for (childNum <- 0 to children.length) {//state update and output when current state corresponds to all child FSMs before the last one
                when (currentState === UInt(childNum)) {
                    childFSMActiveWires(childNum) := Bool(true)
                    when (childFSMDoneWires(childNum)) {
                        if (childNum < children.length - 1) {
                            when (childFSMOccurredWires(childNum)) {
                                nextState := currentState + UInt(1)
                            }.otherwise {
                                done := Bool(true)
                                nextState := UInt(0)
                            }
                        } else {
                            done := Bool(true)
                            nextState := UInt(0)
                            when (childFSMOccurredWires(children.length - 1)) {
                                occurred := Bool(true)
                            }
                        }
                    }
                }
            }
        }

        return (active, done, occurred)
    }
}
