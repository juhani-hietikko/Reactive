package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "or 4")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "or 4")
  }
  
  test("demux with 0 control wires") {
    val in, out = new Wire
    demux(in, Nil, List(out))
    
    in.setSignal(false)
    run
    assert(out.getSignal === false, "false")
    
    in.setSignal(true)
    run
    assert(out.getSignal === true, "true")
  }
  
  test("demux with 1 control wire") {
    val in, out0, out1, c = new Wire
    demux(in, List(c), List(out0, out1))
    
    in.setSignal(false)
    c.setSignal(false)
    run
    assert(out0.getSignal === false, "false-false out0")
    assert(out1.getSignal === false, "false-false out1")
    
    in.setSignal(true)
    c.setSignal(false)
    run
    assert(out0.getSignal === true, "true-false out0")
    assert(out1.getSignal === false, "true-false out1")
    
    in.setSignal(false)
    c.setSignal(true)
    run
    assert(out0.getSignal === false, "false-true out0")
    assert(out1.getSignal === false, "false-true out1")
    
    in.setSignal(true)
    c.setSignal(true)
    run
    assert(out0.getSignal === false, "true-true out0")
    assert(out1.getSignal === true, "true-true out1")
  }
  
  test("demux with 2 control wires") {
    val in, out0, out1, out2, out3, c0, c1 = new Wire
    demux(in, List(c0, c1), List(out0, out1, out2, out3))
    
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(out0.getSignal === false, "true-false-true out0")
    assert(out1.getSignal === true, "true-false-true out1")
    assert(out2.getSignal === false, "true-false-true out2")
    assert(out3.getSignal === false, "true-false-true out3")
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(out0.getSignal === false, "true-true-true out0")
    assert(out1.getSignal === false, "true-true-true out1")
    assert(out2.getSignal === false, "true-true-true out2")
    assert(out3.getSignal === true, "true-true-true out3")
  }
  
  test("demux with 3 control wires") {
    val in, out0, out1, out2, out3, out4, out5, out6, out7, c0, c1, c2 = new Wire
    demux(in, List(c0, c1, c2), List(out0, out1, out2, out3, out4, out5, out6, out7))
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(false)
    c2.setSignal(true)
    run
    assert(out0.getSignal === false, "out0")  // 000
    assert(out1.getSignal === false, "out1")   // 001
    assert(out2.getSignal === false, "out2")  // 010
    assert(out3.getSignal === false, "out3")  // 011
    assert(out4.getSignal === false,"out4")   // 100
    assert(out5.getSignal === true, "out5")  // 101
    assert(out6.getSignal === false, "out6")  // 110
    assert(out7.getSignal === false, "out7")   // 111
  }
}
