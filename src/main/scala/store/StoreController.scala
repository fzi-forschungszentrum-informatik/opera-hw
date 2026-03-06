/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.store

import chisel3._
import chisel3.util._
import opera.{OPERAConfig, BusInfo} 
import opera.scratchpad.{ScratchpadReadIO} 
import opera.axi.{SimpleAXI4WriteIO} 
import opera.instructions.StoreInstruction

class StoreControllerIO(
    outputBus: BusInfo,
    max_rows_per_load_store: Int
    ) extends Bundle {

    // Connection to the scratchpads
    val output_bus = new ScratchpadReadIO(outputBus)

    // Instruction to execute
    val instruction = Flipped(Decoupled(new StoreInstruction(outputBus.totalRows, max_rows_per_load_store)))

    val done = Output(Bool())
} 

class StoreController[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T,U,V])
    extends Module {

  val io = IO(new StoreControllerIO(
    config.outputBus,
    config.max_rows_per_load_store
  ))
  val axi = IO(new SimpleAXI4WriteIO(
    addrWidth = config.axi_addr_width,
    dataWidth = config.axi_data_width
  ))

  //-----------------------------------------------------
  // State machine
  //-----------------------------------------------------
  val sIdle :: sSendAW :: sSendReq :: sWaitResp :: sSendW :: sWaitB :: sNextRow :: Nil = Enum(7)
  val state = RegInit(sIdle)

  val inst = Module(new Queue(io.instruction.bits.cloneType, entries=1))

  inst.io.enq <> io.instruction

  val rowCnt  = Reg(UInt(16.W))
  val beatCnt = Reg(UInt(16.W))

  val addrReg = Reg(UInt(64.W))
  val dataReg = Reg(io.output_bus.resp.bits.data.cloneType)

  val beatsPerRow = (config.outputBus.busWidth / config.axi_data_width)
  val axi_data_bytes = config.axi_data_width / 8

  val outputSlices = Seq.tabulate(beatsPerRow) { i =>
    // Extract bits for beat i
    dataReg((i+1)*config.axi_data_width - 1, i*config.axi_data_width)
  }
  val outputBeatData = Mux1H(
    UIntToOH(beatCnt, beatsPerRow).asBools,
    outputSlices
  )

  //-----------------------------------------------------
  // Defaults
  //-----------------------------------------------------
  io.done := false.B
  inst.io.deq.ready := false.B
  when(state === sIdle){
    inst.io.deq.ready := true.B
  } 

  io.output_bus.req.valid := false.B
  io.output_bus.req.bits.addr := 0.U

  io.output_bus.resp.ready := false.B

  axi.aw.valid := false.B
  axi.aw.bits.addr  := 0.U
  axi.aw.bits.len   := 0.U
  axi.aw.bits.size  := log2Ceil(config.axi_data_width/8).U
  axi.aw.bits.burst := 1.U

  axi.w.valid := false.B
  axi.w.bits.data  := 0.U
  axi.w.bits.strb :=  VecInit(Seq.fill(axi.w.bits.strb.getWidth)(true.B)).asUInt // TODO: careful with the strobe, as maybe we won't use the full bus width in the future
  axi.w.bits.last  := false.B

  axi.b.ready := false.B

  //-----------------------------------------------------
  // FSM
  //-----------------------------------------------------
  switch(state) {

    //---------------------------------------------------
    // Wait for instruction
    //---------------------------------------------------
    is(sIdle) {
      when(io.instruction.fire) {
        rowCnt  := 0.U
        beatCnt := 0.U
        addrReg := io.instruction.bits.external_addr
        state := sSendAW
      }
    }

    //---------------------------------------------------
    // Send AXI write-address burst
    //---------------------------------------------------
    is(sSendAW) {
      axi.aw.valid := true.B
      axi.aw.bits.addr  := addrReg
      axi.aw.bits.len   := (beatsPerRow - 1).U

      when(axi.aw.valid && axi.aw.ready) {
        beatCnt := 0.U
        state := sSendReq
      }
    }

    //---------------------------------------------------
    // Send a read request to scratchpad
    //---------------------------------------------------
    is(sSendReq) {
      io.output_bus.req.valid := true.B
      io.output_bus.req.bits.addr := inst.io.deq.bits.scratchpad_addr + rowCnt

      when(io.output_bus.req.fire) {
        state := sWaitResp
      }
    }

    //---------------------------------------------------
    // Wait for scratchpad response
    //---------------------------------------------------
    is(sWaitResp) {
      io.output_bus.resp.ready := true.B

      when(io.output_bus.resp.fire) {
        // Got one beat from scratchpad
        state := sSendW
        dataReg := io.output_bus.resp.bits.data
      }
    }

    //---------------------------------------------------
    // Send write beat to AXI W channel
    //---------------------------------------------------
    is(sSendW) {
      axi.w.valid := true.B
      axi.w.bits.data  := outputBeatData
      axi.w.bits.last  := (beatCnt === (beatsPerRow - 1).U)

      when(axi.w.valid && axi.w.ready) {
        when(axi.w.bits.last) {
          state := sWaitB
        }.otherwise {
          beatCnt := beatCnt + 1.U
          //state := sSendReq  // request next beat from scratchpad
        }
      }
    }

    //---------------------------------------------------
    // Wait for AXI write response
    //---------------------------------------------------
    is(sWaitB) {
      axi.b.ready := true.B

      when(axi.b.ready && axi.b.valid) {
        state := sNextRow
      }
    }

    //---------------------------------------------------
    // Next row or finish
    //---------------------------------------------------
    is(sNextRow) {
      rowCnt := rowCnt + 1.U
      addrReg := addrReg + inst.io.deq.bits.stride

      when(rowCnt === inst.io.deq.bits.rows - 1.U) {
        state := sIdle
        io.done := true.B
      }.otherwise {
        state := sSendAW
      }
    }
  }
}
