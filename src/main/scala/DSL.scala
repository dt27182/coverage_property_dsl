package CoveragePropertyDSL

import Chisel._
import scala.collection.mutable._

object seq {
    def apply(chiselWire: Bool): AtomicSequence = new AtomicSequence(chiselWire)
}

object cover {
    def apply(implication: ImplicationStatement): (UInt, UInt) = genMonitorFSM(new CoverageStatement(implication))
}

abstract class Sequence {
    def ##(numDelayedCycles: Int): DelaySequence = new DelaySequence(this, seq(Bool()), numDelayedCycles)    
    //def ##(numDelayedCycles: Int, rhs: Sequence) = new DelaySequence(this, rhs, numDelayedCycles)    
    def |=>(thenSeq: Sequence): ImplicationStatement = new ImplicationStatement(this, thenSeq)
}

class AtomicSequence(val chiselWire: Bool) extends Sequence

class DelaySequence(val lhs: Sequence, var rhs: Sequence, val numDelayedCycles: Int) extends Sequence {
    def ##(rhs: Sequence): DelaySequence = {
        this.rhs = rhs
        this
    }
    assert (numDelayedCycles > 0)
}

class ImplicationStatement(val ifSequence: Sequence, val thenSequence: Sequence)

class CoverageStatement(val implicationStatement: ImplicationStatement)

object genMonitorFSM {
    def apply(sequence: Sequence): (Bool, Bool, Bool) = {
        sequence match {
            case seq: AtomicSequence => genMonitorFSM(seq)
            case seq: DelaySequence => genMonitorFSM(seq)
            case _ => {
                assert(false)
                (Bool(), Bool(), Bool())
            }
        }
    }

    def apply(atomicSequence: AtomicSequence): (Bool, Bool, Bool) = {
        val active = Bool()
        val occurred = Bool()
        occurred := atomicSequence.chiselWire

        val done = Bool()
        done := Bool(false)
        when(active) {
            done := Bool(true)
        }

        return (active, done, occurred)
    }

    def apply(delaySequence: DelaySequence): (Bool, Bool, Bool) = {
        val (lhsActive, lhsDone, lhsOccurred) = genMonitorFSM(delaySequence.lhs)
        val (rhsActive, rhsDone, rhsOccurred) = genMonitorFSM(delaySequence.rhs)


        //FSM input signal
        val active = Bool()

        //FSM state reset and update
        val runningLeftHandSide = 0
        val runningRightHandSide = delaySequence.numDelayedCycles
        val currentState = Reg(init = UInt(runningLeftHandSide, width = 32))
        when (active) {
            when (currentState === UInt(runningLeftHandSide)) {
                when (lhsDone) {
                    when (lhsOccurred) {
                        currentState := UInt(runningLeftHandSide + 1)
                    }
                }
            }
            if (delaySequence.numDelayedCycles > 1) {
                for (i <- 1 to (delaySequence.numDelayedCycles - 1)) {
                    when (currentState === UInt(i)) {
                         currentState := currentState + UInt(1) 
                    }
                }
            }
            when (currentState === UInt(runningRightHandSide)) {
                when (rhsDone) {
                    currentState := UInt(runningLeftHandSide)
                }
            }
        }

        //lhs monitor FSM and lhs monitor FSM control signals
        lhsActive := Bool(false)
        rhsActive := Bool(false)
        when (active) {
            when (currentState === UInt(runningLeftHandSide)) {
                lhsActive := Bool(true)
            }
            when (currentState === UInt(runningRightHandSide)) {
                rhsActive := Bool(true)
            }
        }

        //FSM output signals
        val done = Bool()
        val occurred = Bool()
        done := Bool(false)
        occurred := Bool(false)
        when (active) {
            when (currentState === UInt(runningLeftHandSide)) {
                when (lhsDone) {
                    when (!lhsOccurred) {
                        done := Bool(true)
                    }
                }
            }
            when (currentState === UInt(runningRightHandSide)) {
                when (rhsDone) {
                    done := Bool(true)
                    when (rhsOccurred) {
                        occurred := Bool(true)
                    }
                }
            }
        }

        return (active, done, occurred)
    }


    def apply(implicationStatement: ImplicationStatement): (Bool, Bool, Bool, Bool) = {
        //gen children FSMs
        val (ifSeqFSMActive, ifSeqFSMDone, ifSeqFSMOccurred) = genMonitorFSM(implicationStatement.ifSequence)
        val (thenSeqFSMActive, thenSeqFSMDone, thenSeqFSMOccurred) = genMonitorFSM(implicationStatement.thenSequence)

        //FSM input signal
        val active = Bool()

        //FSM state reset and update
        val checkingIfSeq = 0
        val checkingThenSeq = 1
        val currentState = Reg(init = UInt(checkingIfSeq))
        when(active) {
            when(currentState === UInt(checkingIfSeq)) {
                when(ifSeqFSMDone) {
                    when(ifSeqFSMOccurred) {
                        currentState := UInt(checkingThenSeq)
                    }
                }
            }.elsewhen(currentState === UInt(checkingThenSeq)) {
                when(thenSeqFSMDone) {
                    currentState := UInt(checkingIfSeq)
                }
            }
        }

        //child FSM control signals
        ifSeqFSMActive := Bool(false)
        thenSeqFSMActive := Bool(false)
        when(active) {
            when(currentState === UInt(checkingIfSeq)) {
                ifSeqFSMActive := Bool(true)
            }.elsewhen(currentState === UInt(checkingThenSeq)) {
                thenSeqFSMActive := Bool(true)
            }
        }

        //FSM output signals
        val done = Bool()
        val attempted = Bool()
        val matched = Bool()
        done := Bool(false)
        attempted := Bool(false)
        matched := Bool(false)
        when(active) {
            when(currentState === UInt(checkingIfSeq)) {
                when(ifSeqFSMDone) {
                    when(ifSeqFSMOccurred) {
                        attempted := Bool(true)
                    }.otherwise {
                        done := Bool(true)
                    }
                }
            }.elsewhen(currentState === UInt(checkingThenSeq)) {
                when(thenSeqFSMDone) {
                    done := Bool(true)
                    when(thenSeqFSMOccurred) {
                        matched := Bool(true)
                    }
                }
            }
        }
        
        return (active, done, attempted, matched)
    }

    def apply(coverageStatement: CoverageStatement): (UInt, UInt) = {
        val monitorFSMActiveBitVector = Bits(width = getMaxRunCycles(coverageStatement.implicationStatement))
        val monitorFSMDoneBitVector = Bits(width = getMaxRunCycles(coverageStatement.implicationStatement))
        val monitorFSMAttemptedBitVector = Bits(width = getMaxRunCycles(coverageStatement.implicationStatement))
        val monitorFSMMatchedBitVector = Bits(width = getMaxRunCycles(coverageStatement.implicationStatement))
        monitorFSMActiveBitVector := Bits(0)
        monitorFSMDoneBitVector := Bits(0)
        monitorFSMAttemptedBitVector := Bits(0)
        monitorFSMMatchedBitVector := Bits(0)
        for (i <- 1 to getMaxRunCycles(coverageStatement.implicationStatement)) {
            val (active, done, attempted, matched) = genMonitorFSM(coverageStatement.implicationStatement)
            active := monitorFSMActiveBitVector(i - 1)
            monitorFSMDoneBitVector(i - 1) := done
            monitorFSMAttemptedBitVector(i - 1) := attempted
            monitorFSMMatchedBitVector(i - 1) := matched
        }
        
        //manage update of monitor free list
        val monitorFreeList = Reg(init = ~Bits(0, width = getMaxRunCycles(coverageStatement.implicationStatement)) )
        val nextFreeMonitorOH = PriorityEncoderOH(monitorFreeList)
        monitorFreeList := (monitorFreeList ^ nextFreeMonitorOH ) | monitorFSMDoneBitVector

        //manage monitor FSM active inputs
        monitorFSMActiveBitVector := nextFreeMonitorOH | (~monitorFreeList)

        //manage num attempted and num matched counters
        val numAttempted = Reg(init = UInt(0, width=32))
        val numMatched = Reg(init = UInt(0, width=32))
        var nextNumAttempted = UInt(0, width=32)
        var nextNumMatched = UInt(0, width=32)
        for (i <- 0 to getMaxRunCycles(coverageStatement.implicationStatement) - 1) {
            nextNumAttempted = nextNumAttempted + monitorFSMAttemptedBitVector(i)
            nextNumMatched = nextNumMatched + monitorFSMMatchedBitVector(i)
        }
        numAttempted := nextNumAttempted
        numMatched := nextNumMatched

        return (nextNumAttempted, nextNumMatched)
    }
}

object getMaxRunCycles {
    def apply(sequence: Sequence): Int = {
        sequence match {
            case seq: AtomicSequence => getMaxRunCycles(seq)
            case seq: DelaySequence => getMaxRunCycles(seq)
            case _ => {
                assert(false)
                -1
            }
        }
    }

    def apply(atomicSequence: AtomicSequence): Int = 1

    def apply(delaySequence: DelaySequence): Int = getMaxRunCycles(delaySequence.lhs) + (delaySequence.numDelayedCycles - 1) + getMaxRunCycles(delaySequence.rhs)

    def apply(implicationStatement: ImplicationStatement): Int = getMaxRunCycles(implicationStatement.ifSequence) + getMaxRunCycles(implicationStatement.thenSequence)
}

