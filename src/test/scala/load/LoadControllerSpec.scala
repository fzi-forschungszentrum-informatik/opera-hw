/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.test

import chisel3._
import chisel3.testing.HasTestingDirectory
import chisel3.simulator.{HasSimulator, Settings} 
import svsim.{CommonCompilationSettings, CommonSettingsModifications, CommonSimulationSettings, PlusArg} 
import svsim.verilator.Backend.CompilationSettings.TraceKind
import chisel3.simulator.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import opera.load.LoadController
import opera.OPERAConfigs

class LoadControllerSpec extends AnyFlatSpec with CustomChiselSim with Matchers {

  behavior of "LoadController"

  it should "load 3 rows from external memory into the scratchpad" in {

    simulate(new LoadController(OPERAConfigs.defaultConfig)) { dut =>
      enableWaves()

      //---------------------------------------------------------------
      // Useful shortcuts
      //---------------------------------------------------------------
      val axi = dut.axi
      val sp  = dut.io.input_bus  // scratchpad write bus

      //---------------------------------------------------------------
      // Test parameters
      //---------------------------------------------------------------
      val rowsToLoad = 3
      val beatsPerRow = OPERAConfigs.defaultConfig.inputBus.busWidth / OPERAConfigs.defaultConfig.axi_data_width
      var rowCounter = 0
      val stride = 64      // example

      // Simulated external memory: map each beat index -> data
      val externalMem = scala.collection.mutable.Map[Int, BigInt]()

      for (row <- 0 until rowsToLoad) {
        for (beat <- 0 until beatsPerRow) {
          val globalIndex = row * beatsPerRow + beat
          externalMem(globalIndex) = BigInt(0x1000 + globalIndex)
        }
      }

      // Simulated scratchpad
      val scratchpad = scala.collection.mutable.Map[Int, BigInt]()
      val scratchpadBeats = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)

      // Reset
      dut.reset.poke(true.B)
      dut.io.instruction.valid.poke(false.B)
      dut.clock.step(32)
      dut.reset.poke(false.B)
      dut.clock.step(1)

      //---------------------------------------------------------------
      // Feed LoadInstruction
      //---------------------------------------------------------------
      dut.io.instruction.valid.poke(true)
      dut.io.instruction.bits.scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.external_addr.poke(0.U)
      dut.io.instruction.bits.rows.poke(rowsToLoad.U)
      dut.io.instruction.bits.stride.poke(stride.U)

      // Wait until controller accepts it
      while (!dut.io.instruction.ready.peekBoolean()) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.instruction.valid.poke(false)
      dut.clock.step()

      //---------------------------------------------------------------
      // AXI read monitor state
      //---------------------------------------------------------------
      var currentRequestedRow = 0
      var currentRespondedRow = 0
      var currentBeat = 0
      var send_r = false

      var current_sp_write = 0
      var total_sp_writes = 0
      var expected_sp_addr = 0

      //---------------------------------------------------------------
      // Main test loop
      //---------------------------------------------------------------
      var dataWrittenToSP = false
      for (_ <- 0 until 200) {

        //-------------------------------------------------------------
        // AW channel (read address)
        //-------------------------------------------------------------
        if (axi.ar.valid.peekBoolean()) {
          // Accept the address
          axi.ar.ready.poke(true)
          currentBeat = 0
          send_r = true

          // Optionally check correctness:
          val expectedAddr = BigInt(currentRequestedRow) * stride
          val gotAddr = axi.ar.bits.addr.peek().litValue
          require(gotAddr == expectedAddr, s"AW address mismatch: expected $expectedAddr, got $gotAddr")

          currentRequestedRow += 1

        } else {
          axi.ar.ready.poke(false)
        }

        //-------------------------------------------------------------
        // R channel (read data)
        //-------------------------------------------------------------
        if (send_r && !axi.ar.valid.peekBoolean()) {
          // Only respond when LoadController actually requests data:
          if (currentBeat < beatsPerRow) {
            // Provide the next beat
            val globalBeatIndex = currentRespondedRow * beatsPerRow + currentBeat
            val data = externalMem(globalBeatIndex)

            axi.r.valid.poke(true)
            axi.r.bits.data.poke(data.U)
            if(currentBeat == beatsPerRow - 1){
              axi.r.bits.last.poke(true.B)
            } else{
              axi.r.bits.last.poke(false.B)
            } 

            // Move counters
            currentBeat += 1

          } else {
            // Already responded
            currentRespondedRow += 1
            axi.r.valid.poke(false)
            send_r = false
          }
        } else {
          axi.r.valid.poke(false)
        }

        //-------------------------------------------------------------
        // Scratchpad write checking (mask-aware)
        //-------------------------------------------------------------
        if (sp.req.valid.peekBoolean()) {

          total_sp_writes += 1

          val rowIdx = sp.req.bits.addr.peek().litValue.toInt

          require(rowIdx == expected_sp_addr, f"Expected scratchpad addr 0x$expected_sp_addr%X but got 0x$rowIdx%X")
          current_sp_write = current_sp_write + 1
          if(current_sp_write == beatsPerRow){
            current_sp_write = 0
            expected_sp_addr = expected_sp_addr + 1
          } 

          dataWrittenToSP = true

          val wdata    = sp.req.bits.data.peek().litValue
          val wmask    = sp.req.bits.mask.peek().litValue   // mask is in bytes!
          val rowBytes = OPERAConfigs.defaultConfig.inputBus.busWidth / 8
          val beatBytes = OPERAConfigs.defaultConfig.axi_data_width / 8

          // Allocate scratchpad if first time
          if (!scratchpad.contains(rowIdx)) {
            scratchpad(rowIdx) = BigInt(0)
          }

          // Merge masked write into the accumulated row buffer
          //
          // mask is byte-granular: bit[i]==1 => byte i is written
          //
          var curRow = scratchpad(rowIdx)
          for (byte <- 0 until rowBytes) {
            val maskBit = (wmask >> byte) & 1
            if (maskBit == 1) {
              val newByte = (wdata >> (byte*8)) & 0xff
              val cleared = curRow & ~(BigInt(0xff) << (byte*8))
              curRow = cleared | (newByte << (byte*8))
            }
          }

          scratchpad(rowIdx) = curRow

          // If we received the final beat for this row, compare
          val beatsSoFar = scratchpadBeats(rowIdx) + 1
          scratchpadBeats(rowIdx) = beatsSoFar

          if (beatsSoFar == beatsPerRow) {
            // Build expected full row by concatenating beat data
            var expected = BigInt(0)
            for (b <- beatsPerRow-1 to 0 by -1) {
              val globalBeat = rowIdx * beatsPerRow + b
              expected = (expected << (beatBytes*8)) | externalMem(globalBeat)
            }

            val got = scratchpad(rowIdx)
            require(got == expected,
            f"Scratchpad row $rowIdx mismatch:\nexpected=0x$expected%X\ngot=0x$got%X")
          }
        }

        dut.clock.step()
      }

      require(currentRequestedRow == rowsToLoad, s"AXI interface should have requested $rowsToLoad rows!")
      require(total_sp_writes == rowsToLoad*beatsPerRow, s"Scratchpad bus should have written ${rowsToLoad*beatsPerRow} times but wrote $total_sp_writes!")
      dataWrittenToSP shouldBe true
    }
  }
}
