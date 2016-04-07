package CoveragePropertyDSL

import Chisel._
import scala.collection.mutable._

class AtomicSequenceModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val (active, done, occurred) = genMonitorFSM(seq(io.chiselWire))
    active := io.active
    io.done := done
    io.occurred := occurred
}

class AtomicSequenceOccurredTester(c: AtomicSequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    poke(c.io.active, 1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 1)
}

class AtomicSequenceNotOccurredTester(c: AtomicSequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 0)
    poke(c.io.active, 1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)
}

class DelaySequenceModule0 extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val (active, done, occurred) = genMonitorFSM(seq(io.chiselWire) ## 2 ## seq(io.chiselWire))
    active := io.active
    io.done := done
    io.occurred := occurred
}

class DelaySequence0OccurredTester(c: DelaySequenceModule0) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    poke(c.io.active, 1)

    step(1)
    expect(c.io.done, 0)

    step(1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 1)

    poke(c.io.active, 0)
    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class DelaySequence0NotOccurredTester(c: DelaySequenceModule0) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    poke(c.io.active, 1)
    expect(c.io.done, 0)

    step(1)
    expect(c.io.done, 0)
    poke(c.io.chiselWire, 0)

    step(1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)

    poke(c.io.active, 0)
    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class DelaySequenceModule1 extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val atomic0 = new AtomicSequence(io.chiselWire)
    val atomic1 = new AtomicSequence(!io.chiselWire)
    val delay0 = new DelaySequence(atomic0, atomic1, 2)
    val delay1 = new DelaySequence(delay0, atomic0, 3)

    val (active, done, occurred) = genMonitorFSM(delay1)
    active := io.active
    io.done := done
    io.occurred := occurred
}

class DelaySequence1OccurredTester(c: DelaySequenceModule1) extends Tester(c) {
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//second check
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//third check
    expect(c.io.done, 1)
    expect(c.io.occurred, 1)

    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class DelaySequence1NotOccurredTester0(c: DelaySequenceModule1) extends Tester(c) {
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//second check
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)

    step(1)
    poke(c.io.active, 0)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class DelaySequence1NotOccurredTester1(c: DelaySequenceModule1) extends Tester(c) {
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//second check
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//third check
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)

    step(1)
    poke(c.io.active, 0)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class ImplicationStatementModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val attempted = Bool(OUTPUT)
        val matched = Bool(OUTPUT)
    }
    val (active, done, attempted, matched) = genMonitorFSM(seq(io.chiselWire) ## 2 ## seq(!io.chiselWire) ## 3 ## seq(io.chiselWire) |=> seq(io.chiselWire) ## 2 ## seq(!io.chiselWire) ## 3 ## seq(io.chiselWire))
    
    active := io.active
    io.done := done
    io.attempted := attempted
    io.matched := matched
}

class ImplicationStatementTester0(c: ImplicationStatementModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//second check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//third check
    expect(c.io.done, 0)
    expect(c.io.attempted, 1)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//second check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//third check
    expect(c.io.done, 1)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 1)

    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class ImplicationStatementTester1(c: ImplicationStatementModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//second check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//third check
    expect(c.io.done, 0)
    expect(c.io.attempted, 1)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//second check
    expect(c.io.done, 1)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class ImplicationStatementTester2(c: ImplicationStatementModule) extends Tester(c) {
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 0)//first check
    expect(c.io.done, 1)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 0)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class ImplicationStatementTester3(c: ImplicationStatementModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//first check
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)
    expect(c.io.done, 0)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 1)
    poke(c.io.chiselWire, 1)//second check
    expect(c.io.done, 1)
    expect(c.io.attempted, 0)
    expect(c.io.matched, 0)

    step(1)
    poke(c.io.active, 0)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class CoverageStatementModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val numAttempted = UInt(OUTPUT)
        val numMatched = UInt(OUTPUT)
    }
    val (numAttempted, numMatched) = cover(seq(io.chiselWire) ## 2 ## seq(!io.chiselWire) ## 3 ## seq(io.chiselWire) |=> seq(io.chiselWire) ## 2 ## seq(!io.chiselWire) ## 3 ## seq(io.chiselWire))
    
    io.numAttempted := numAttempted
    io.numMatched := numMatched
}

class CoverageStatementTester(c: CoverageStatementModule) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    step(1)
    step(1)
    poke(c.io.chiselWire, 0)
    step(1)
    step(1)
    step(1)
    poke(c.io.chiselWire, 1)
    step(1)
    poke(c.io.chiselWire, 1)
    step(1)
    step(1)
    poke(c.io.chiselWire, 0)
    step(1)
    step(1)
    step(1)
    poke(c.io.chiselWire, 1)
    expect(c.io.numAttempted, 1)
    expect(c.io.numMatched, 1)
}

object TestRunner {
  def main(args: Array[String]): Unit = {
    val commandArgs = args.slice(1, args.length)

    chiselMainTest(commandArgs, () => Module(new AtomicSequenceModule())) {
      c => new AtomicSequenceOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new AtomicSequenceModule())) {
      c => new AtomicSequenceNotOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule0())) {
      c => new DelaySequence0OccurredTester(c) }
    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule0())) {
      c => new DelaySequence0NotOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule1())) {
      c => new DelaySequence1OccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule1())) {
      c => new DelaySequence1NotOccurredTester0(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule1())) {
      c => new DelaySequence1NotOccurredTester1(c) }

    chiselMainTest(commandArgs, () => Module(new ImplicationStatementModule())) {
      c => new ImplicationStatementTester0(c) }

    chiselMainTest(commandArgs, () => Module(new ImplicationStatementModule())) {
      c => new ImplicationStatementTester1(c) }

    chiselMainTest(commandArgs, () => Module(new ImplicationStatementModule())) {
      c => new ImplicationStatementTester2(c) }

    chiselMainTest(commandArgs, () => Module(new ImplicationStatementModule())) {
      c => new ImplicationStatementTester3(c) }

    chiselMainTest(commandArgs, () => Module(new CoverageStatementModule())) {
      c => new CoverageStatementTester(c) }
  }
}
