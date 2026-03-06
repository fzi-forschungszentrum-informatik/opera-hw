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
import opera.execute.computation_units.{ProcessingElement, WeightStationarySystolicArray} 
import opera.types.DataflowType

class ProcessingElementTest extends AnyFunSpec with CustomChiselSim {
  describe("ProcessingElement"){
    it("should load and store weight correctly"){
        simulate(new ProcessingElement(SInt(16.W), SInt(16.W), SInt(32.W))) { dut =>
        // Initialize
        dut.io.weight_in.poke(5.S)
        dut.io.weight_load_in.poke(true.B)
        dut.io.data_in.poke(0.S)
        dut.io.data_valid_in.poke(false.B)
        dut.io.partial_sum_in.poke(0.S)
        dut.io.partial_sum_valid_in.poke(false.B)
        
        dut.clock.step()
        
        // Weight should be loaded
        dut.io.weight_load_in.poke(false.B)
        dut.io.data_in.poke(3.S)
        dut.io.data_valid_in.poke(true.B)
        dut.io.partial_sum_in.poke(10.S)
        dut.io.partial_sum_valid_in.poke(true.B)
        
        dut.clock.step()
        
        // Check MAC operation: 10 + (5 * 3) = 25
        dut.io.partial_sum_out.expect(25.S)
        dut.io.partial_sum_valid_out.expect(true.B)
        dut.io.data_out.expect(3.S)
        dut.io.data_valid_out.expect(true.B)
        }
    } 

    it("should perform MAC operation correctly with multiple values"){
        simulate(new ProcessingElement(SInt(16.W), SInt(16.W), SInt(32.W))) { dut =>
      // Load weight = 7
      dut.io.weight_in.poke(7.S)
      dut.io.weight_load_in.poke(true.B)
      dut.io.data_in.poke(0.S)
      dut.io.data_valid_in.poke(false.B)
      dut.io.partial_sum_in.poke(0.S)
      dut.io.partial_sum_valid_in.poke(false.B)
      dut.clock.step()
      
      dut.io.weight_load_in.poke(false.B)
      
      // Test case 1: 0 + (7 * 2) = 14
      dut.io.data_in.poke(2.S)
      dut.io.data_valid_in.poke(true.B)
      dut.io.partial_sum_in.poke(0.S)
      dut.io.partial_sum_valid_in.poke(true.B)
      dut.clock.step()
      dut.io.partial_sum_out.expect(14.S)
      
      // Test case 2: 5 + (7 * 4) = 33
      dut.io.data_in.poke(4.S)
      dut.io.partial_sum_in.poke(5.S)
      dut.clock.step()
      dut.io.partial_sum_out.expect(33.S)
      
      // Test case 3: -10 + (7 * -3) = -31
      dut.io.data_in.poke(-3.S)
      dut.io.partial_sum_in.poke(-10.S)
      dut.clock.step()
      dut.io.partial_sum_out.expect(-31.S)
    }
    } 

    it("should propagate data correctly"){
        simulate(new ProcessingElement(SInt(16.W), SInt(16.W), SInt(32.W))) { dut =>
      // Load weight
      dut.io.weight_in.poke(1.S)
      dut.io.weight_load_in.poke(true.B)
      dut.io.data_in.poke(0.S)
      dut.io.data_valid_in.poke(false.B)
      dut.io.partial_sum_in.poke(0.S)
      dut.io.partial_sum_valid_in.poke(false.B)
      dut.clock.step()
      
      dut.io.weight_load_in.poke(false.B)
      
      // Send data through
      dut.io.data_in.poke(42.S)
      dut.io.data_valid_in.poke(true.B)
      dut.io.partial_sum_in.poke(0.S)
      dut.io.partial_sum_valid_in.poke(true.B)
      dut.clock.step()
      
      // Data should appear at output (with 1 cycle delay)
      dut.io.data_out.expect(42.S)
      dut.io.data_valid_out.expect(true.B)
    }
    } 
  }     
}

class WeightStationarySystolicArrayTest extends AnyFunSpec with ChiselSim {
  describe("WeightStationarySystolicArray")
  {
    it("should load weights correctly"){
      val config = ComputationUnitConfig(
        N = 2,
        K = 3,
        ADataType = SInt(16.W),
        BDataType = SInt(16.W),
        DDataType = SInt(32.W),
        supportedDataflows = Seq(DataflowType.WS)
      )
    
    simulate(new WeightStationarySystolicArray(config)) { dut =>
      // Initialize inputs
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      dut.io.in_d.valid.poke(false.B)
      dut.io.out_c.ready.poke(true.B)
      
      dut.clock.step(2)
      
      // Load weights column by column
      // Column 0: weights [1, 2, 3]
      dut.io.in_b.valid.poke(true.B)
      dut.io.in_b.bits.stationary.poke(true.B)
      dut.io.in_b.bits.data(0).poke(1.S)
      dut.io.in_b.bits.data(1).poke(2.S)
      dut.io.in_b.bits.data(2).poke(3.S)
      
      dut.clock.step()
      
      // Column 1: weights [4, 5, 6]
      dut.io.in_b.bits.data(0).poke(4.S)
      dut.io.in_b.bits.data(1).poke(5.S)
      dut.io.in_b.bits.data(2).poke(6.S)
      
      dut.clock.step()
      
      // Column 2: weights [7, 8, 9]
      dut.io.in_b.bits.data(0).poke(7.S)
      dut.io.in_b.bits.data(1).poke(8.S)
      dut.io.in_b.bits.data(2).poke(9.S)
      
      dut.clock.step()
      
      dut.io.in_b.valid.poke(false.B)
      dut.clock.step(1)
      
      // Weights should be loaded, system should be in compute state
      dut.io.in_a.ready.expect(true.B)
    }
    } 

    it("should perform simple matrix-matrix multiplication"){
      val config = ComputationUnitConfig(
        N = 2,
        K = 3,
        ADataType = SInt(16.W),
        BDataType = SInt(16.W),
        DDataType = SInt(32.W),
        supportedDataflows = Seq(DataflowType.WS)
      )

    // | 1 | 2 | 3 |   | 1 | 2 | 3 |   | 1 | 3 |   | 15 | 35 | 
    // | 4 | 5 | 6 | * | 4 | 5 | 6 | + | 2 | 4 | = | 34 | 81 | 

    simulate(new WeightStationarySystolicArray(config)) { dut =>
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      dut.io.in_d.valid.poke(false.B)
      dut.io.out_c.ready.poke(true.B)
      
      dut.clock.step(2)
      
      // Load weights: Row 0: [1, 2, 3], Row 1: [4, 5, 6]
      dut.io.in_b.valid.poke(true.B)
      dut.io.in_b.bits.stationary.poke(true.B)
      
      dut.io.in_b.bits.data(0).poke(1.S)
      dut.io.in_b.bits.data(1).poke(2.S)
      dut.io.in_b.bits.data(2).poke(3.S)
      dut.clock.step()
      dut.io.in_b.bits.data(0).poke(4.S)
      dut.io.in_b.bits.data(1).poke(5.S)
      dut.io.in_b.bits.data(2).poke(6.S)
      dut.clock.step()
      
      dut.io.in_b.valid.poke(false.B)
      dut.clock.step(3)
      
      // Feed input data: [1, 2, 3] for row 0
      dut.io.in_a.valid.poke(true.B)
      dut.io.in_a.bits.stationary.poke(false.B)
      dut.io.in_a.bits.data(0).poke(1.S)
      dut.io.in_a.bits.data(1).poke(2.S)
      dut.io.in_a.bits.data(2).poke(3.S)
      
      // Feed bias: [1, 2]
      dut.io.in_d.valid.poke(true.B)
      dut.io.in_d.bits.stationary.poke(false.B)
      dut.io.in_d.bits.data(0).poke(1.S)
      dut.io.in_d.bits.data(1).poke(2.S)
      
      dut.clock.step()

      // Feed input data: [4, 5, 6] for row 1
      dut.io.in_a.valid.poke(true.B)
      dut.io.in_a.bits.stationary.poke(false.B)
      dut.io.in_a.bits.data(0).poke(4.S)
      dut.io.in_a.bits.data(1).poke(5.S)
      dut.io.in_a.bits.data(2).poke(6.S)
      
      // Feed bias: [3, 4]
      dut.io.in_d.valid.poke(true.B)
      dut.io.in_d.bits.stationary.poke(false.B)
      dut.io.in_d.bits.data(0).poke(3.S)
      dut.io.in_d.bits.data(1).poke(4.S)
      
      dut.clock.step()
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_d.valid.poke(false.B)
      
      // Check if output is available
      var firstOutputReceived = false
      var secondOutputReceived = false
      var maxSteps = 20
      while (!secondOutputReceived && maxSteps > 0) {
        if (dut.io.out_c.valid.peekBoolean()) {
          println(s"Output Row 0: ${dut.io.out_c.bits.data(0).peek()}")
          println(s"Output Row 1: ${dut.io.out_c.bits.data(1).peek()}")
          if(firstOutputReceived){
            secondOutputReceived = true
            dut.io.out_c.bits.data(0).expect(35)
            dut.io.out_c.bits.data(1).expect(81)
          } 
          else{
            firstOutputReceived = true
            dut.io.out_c.bits.data(0).expect(15)
            dut.io.out_c.bits.data(1).expect(34)
          } 
        } else {
          maxSteps -= 1
        }
        dut.clock.step()
      }
      
      assert(secondOutputReceived == true, "Expected condition to be true")
    }
    } 
  } 
}