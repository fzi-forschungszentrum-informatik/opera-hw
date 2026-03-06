/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.test

import chisel3._
import chisel3.simulator.scalatest._
import org.scalatest.funspec.AnyFunSpec
import opera.execute.{ComputationUnitConfig, CUBusPayload} 
import opera.execute.computation_units.{SimpleOSCU} 
import opera.types.DataflowType

class SimpleOSCUTest extends AnyFunSpec with CustomChiselSim {
  describe("SimpleOSCU")
  {
    it("should perform simple vector-vector multiplication"){
    val config = ComputationUnitConfig(
      N = 1,
      K = 3,
      ADataType = SInt(16.W),
      BDataType = SInt(16.W),
      DDataType = SInt(32.W),
      supportedDataflows = Seq(DataflowType.OS)
    )

    // | 1 | 2 | 3 | * | 4 | 5 | 6 | + | 1 | 2 | 3 | * | 4 | 5 | 6 | + | 2 | = | 66 | 

    simulate(new SimpleOSCU(config)) { dut =>
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      dut.io.in_d.valid.poke(false.B)
      dut.io.out_c.ready.poke(true.B)
      
      dut.clock.step(2)

      // Load bias
      dut.io.in_d.valid.poke(true.B)
      dut.io.in_d.bits.stationary.poke(true.B)
      dut.io.in_d.bits.data(0).poke(2.S)

      dut.clock.step(1)

      dut.io.in_d.valid.poke(false.B)
      
      // Load data and weights
      dut.io.in_a.valid.poke(true.B)
      dut.io.in_a.bits.stationary.poke(false.B)
      dut.io.in_a.bits.data(0).poke(1.S)
      dut.io.in_a.bits.data(1).poke(2.S)
      dut.io.in_a.bits.data(2).poke(3.S)

      dut.io.in_b.valid.poke(true.B)
      dut.io.in_b.bits.stationary.poke(false.B)
      dut.io.in_b.bits.data(0).poke(4.S)
      dut.io.in_b.bits.data(1).poke(5.S)
      dut.io.in_b.bits.data(2).poke(6.S)

      dut.clock.step()

      dut.io.in_a.valid.poke(true.B)
      dut.io.in_a.bits.stationary.poke(false.B)
      dut.io.in_a.bits.data(0).poke(1.S)
      dut.io.in_a.bits.data(1).poke(2.S)
      dut.io.in_a.bits.data(2).poke(3.S)

      dut.io.in_b.valid.poke(true.B)
      dut.io.in_b.bits.stationary.poke(false.B)
      dut.io.in_b.bits.data(0).poke(4.S)
      dut.io.in_b.bits.data(1).poke(5.S)
      dut.io.in_b.bits.data(2).poke(6.S)

      dut.clock.step()

      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      
      // Check if output is available
      var firstOutputReceived = false
      var maxSteps = 20
      while (!firstOutputReceived && maxSteps > 0) {
        if (dut.io.out_c.valid.peekBoolean()) {
          println(s"Output Row 0: ${dut.io.out_c.bits.data(0).peek()}")
          firstOutputReceived = true
          dut.io.out_c.bits.data(0).expect(66)
        } else {
          maxSteps -= 1
        }
        dut.clock.step()
      }
      
      assert(firstOutputReceived == true, "Expected condition to be true")
    }
    } 
  } 
}