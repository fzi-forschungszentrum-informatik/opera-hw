/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.test

import chisel3._
import chisel3.simulator.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import opera.store.StoreController
import opera.OPERAConfigs
import opera.types.TensorType

class StoreControllerSpec extends AnyFlatSpec with CustomChiselSim with Matchers {
  behavior of "StoreControllerSpec"

  it should "store 3 rows from scratchpad into external memory using AXI writes" in {

    simulate(new StoreController(OPERAConfigs.defaultConfig)) { dut => 
      val axi = dut.axi
      val sp  = dut.io.output_bus  // scratchpad read bus

      //-------------------------------------------------------------------
      // Test parameters
      //-------------------------------------------------------------------
      val scratchpad_rows = OPERAConfigs.defaultConfig.outputBus.busWidth
      val rowsToStore  = 3
      val beatsPerRow = OPERAConfigs.defaultConfig.outputBus.busWidth / OPERAConfigs.defaultConfig.axi_data_width
      val stride       = 64

      // Fake scratchpad data
      // scratchpad(row, beat) -> data value
      val scratchpadMem = scala.collection.mutable.Map[Int, BigInt]()
      val individualBeat = scala.collection.mutable.Map[(Int, Int), BigInt]()

      for (row <- 0 until scratchpad_rows) {
        var rowValue = BigInt(0)
        for (beat <- 0 until beatsPerRow) {
          val beatVal = BigInt(0x2000 + row * 16 + beat)
          individualBeat((row, beat)) = beatVal
          // pack into the wide row (little-endian beat ordering)
          rowValue = rowValue | (beatVal << (beat * OPERAConfigs.defaultConfig.axi_data_width))
        }
        scratchpadMem(row) = rowValue
      }

      // Will record everything written to "external memory"
      val externalWritten = scala.collection.mutable.Map[(Int,Int), BigInt]()

      //-------------------------------------------------------------------
      // Send StoreInstruction
      //-------------------------------------------------------------------
      dut.io.instruction.valid.poke(true)
      dut.io.instruction.bits.scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.external_addr.poke(0.U)
      dut.io.instruction.bits.rows.poke(rowsToStore.U)
      dut.io.instruction.bits.stride.poke(stride.U)

      dut.clock.step()
      dut.io.instruction.valid.poke(false)

      //-------------------------------------------------------------------
      // AXI write channel bookkeeping
      //-------------------------------------------------------------------
      var currentRow = 0
      var currentBeat = 0

      var send_sp_resp = false

      //-------------------------------------------------------------------
      // Main simulation loop
      //-------------------------------------------------------------------
      for (_ <- 0 until 200) {

        //-----------------------------------------------------------------
        // Handle AW channel (write address)
        //-----------------------------------------------------------------
        if (axi.aw.valid.peekBoolean()) {
          axi.aw.ready.poke(true.B)

          // Validate AW address
          val expectedAddr = BigInt(currentRow) * stride
          val gotAddr      = axi.aw.bits.addr.peek().litValue
          require(
            gotAddr == expectedAddr,
            s"AW addr mismatch at row $currentRow: expected $expectedAddr got $gotAddr"
          )

        } else {
          axi.aw.ready.poke(false.B)
        }

        //-----------------------------------------------------------------
        // Handle scratchpad read req -> resp
        //-----------------------------------------------------------------
        
        // Prepare scratchpad response only when controller is ready
        if (send_sp_resp) {
          // Provide next beat
          val value = scratchpadMem(currentRow)
          send_sp_resp = false

          sp.resp.valid.poke(true.B)
          sp.resp.bits.data.poke(value)

        } else {
          sp.resp.valid.poke(false.B)
        }

        if (sp.req.valid.peekBoolean()) {
          // Accept request
          sp.req.ready.poke(true.B)

          val reqAddr = sp.req.bits.addr.peek().litValue.toInt
          require(reqAddr == currentRow,
            s"Scratchpad requested row $reqAddr, expected $currentRow")

          // After req.fire we need to generate response in next cycles
          send_sp_resp = true
        } else {
          sp.req.ready.poke(false.B)
        }

        

        //-----------------------------------------------------------------
        // Handle W channel (write data beats)
        //-----------------------------------------------------------------
        if (axi.w.valid.peekBoolean() && axi.w.ready.peekBoolean()) {

          val gotData = axi.w.bits.data.peek().litValue

          // Record it
          externalWritten((currentRow, currentBeat)) = gotData

          // Next beat
          currentBeat += 1
          if (currentBeat == beatsPerRow) {
            currentBeat = 0
          }
        }

        // Always ready to accept W channel
        axi.w.ready.poke(true.B)

        //-----------------------------------------------------------------
        // Handle B channel (write response)
        //-----------------------------------------------------------------
        if (axi.b.ready.peekBoolean()) {
          // After we complete a row:
          if (currentBeat == 0) {
            currentRow += 1
            axi.b.valid.poke(true.B)
            axi.b.bits.ok.poke(true.B)
          }
        } else {
          axi.b.valid.poke(false.B)
        }

        //-----------------------------------------------------------------
        dut.clock.step()
      }

      //-------------------------------------------------------------------
      // Final verification
      //-------------------------------------------------------------------
      require(externalWritten.size == rowsToStore * beatsPerRow,
        s"Not all AXI write beats were captured")

      for (row <- 0 until rowsToStore; beat <- 0 until beatsPerRow) {
        val expected = individualBeat((row, beat))
        val got      = externalWritten((row, beat))
        require(
          got == expected,
          s"Row $row beat $beat mismatch: expected=$expected got=$got"
        )
      }
    }
  }
}   