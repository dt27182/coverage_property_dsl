package CoveragePropertyDSL

import Chisel._
import scala.collection.mutable._

class StartSequenceModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val (active, done, occurred) = new StartSequence(io.chiselWire).genFSM()
    active := io.active
    io.done := done
    io.occurred := occurred
}

class StartSequenceOccurredTester(c: StartSequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    poke(c.io.active, 1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 1)
}

class StartSequenceNotOccurredTester(c: StartSequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 0)
    poke(c.io.active, 1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)
}

class DelaySequenceModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val (active, done, occurred) = new DelaySequence(io.chiselWire, 3).genFSM()
    active := io.active
    io.done := done
    io.occurred := occurred
}

class DelaySequenceOccurredTester(c: DelaySequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 1)
    poke(c.io.active, 1)

    step(1)
    expect(c.io.done, 0)
    step(1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 1)
    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class DelaySequenceNotOccurredTester(c: DelaySequenceModule) extends Tester(c) {
    poke(c.io.chiselWire, 0)
    poke(c.io.active, 1)
    expect(c.io.done, 0)

    step(1)
    expect(c.io.done, 0)
    step(1)
    expect(c.io.done, 1)
    expect(c.io.occurred, 0)
    step(1)
    expect(c.io.done, 0)//expects FSM to reset after it asserts done
}

class ConcatSequenceModule extends Module {
    val io = new Bundle {
        val chiselWire = Bool(INPUT)
        val active = Bool(INPUT)
        val done = Bool(OUTPUT)
        val occurred = Bool(OUTPUT)
    }
    val start = new StartSequence(io.chiselWire)
    val delay0 = new DelaySequence(!io.chiselWire, 2)
    val delay1 = new DelaySequence(io.chiselWire, 3)
    val sequences = new ArrayBuffer[AtomicSequence]
    sequences += start
    sequences += delay0
    sequences += delay1

    val (active, done, occurred) = new ConcatSequence(sequences).genFSM()
    active := io.active
    io.done := done
    io.occurred := occurred
}

class ConcatSequenceOccurredTester(c: ConcatSequenceModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
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

class ConcatSequenceNotOccurredTester0(c: ConcatSequenceModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
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

class ConcatSequenceNotOccurredTester1(c: ConcatSequenceModule) extends Tester(c) {
    poke(c.io.active, 1)

    step(1)
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

object TestRunner {
  def main(args: Array[String]): Unit = {
    val commandArgs = args.slice(1, args.length)
    chiselMainTest(commandArgs, () => Module(new StartSequenceModule())) {
      c => new StartSequenceOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new StartSequenceModule())) {
      c => new StartSequenceNotOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule())) {
      c => new DelaySequenceOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new DelaySequenceModule())) {
      c => new DelaySequenceNotOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new ConcatSequenceModule())) {
      c => new ConcatSequenceOccurredTester(c) }

    chiselMainTest(commandArgs, () => Module(new ConcatSequenceModule())) {
      c => new ConcatSequenceNotOccurredTester0(c) }

    chiselMainTest(commandArgs, () => Module(new ConcatSequenceModule())) {
      c => new ConcatSequenceNotOccurredTester1(c) }
  }
}
