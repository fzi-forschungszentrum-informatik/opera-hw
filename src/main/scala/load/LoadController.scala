/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.load

import chisel3._
import chisel3.util._
import opera.{OPERAConfig, BusInfo} 
import opera.scratchpad.{ScratchpadWriteIO} 
import opera.axi.{SimpleAXI4ReadIO} 
import opera.types.TensorTypeHW
import opera.instructions.LoadInstruction

class LoadControllerIO(
    inputBus: BusInfo,
    kernelBus: BusInfo,
    biasBus: BusInfo,
    outputBus: BusInfo,
    max_sram_rows: Int,
    max_rows_per_load_store: Int
    ) extends Bundle {

    // Connection to the scratchpads
    val input_bus = new ScratchpadWriteIO(inputBus)
    val kernel_bus = new ScratchpadWriteIO(kernelBus)
    val bias_bus = new ScratchpadWriteIO(biasBus)

    // Instruction to execute
    val instruction = Flipped(Decoupled(new LoadInstruction(max_sram_rows, max_rows_per_load_store)))

    val done = Output(Bool())
} 

class LoadController[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T,U,V])
  extends Module {

  val io = IO(new LoadControllerIO(
    config.inputBus,
    config.kernelBus,
    config.biasBus,
    config.outputBus,
    config.max_sram_rows,
    config.max_rows_per_load_store
  ))

  val axi = IO(new SimpleAXI4ReadIO(config.axi_addr_width, config.axi_data_width))

  // ------------------------------
  // State Machine Definitions
  // ------------------------------
  val sIdle :: sSendAR :: sReadData :: sNextRow :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // Instruction register
  val inst = Module(new Queue(io.instruction.bits.cloneType, entries=1))

  inst.io.enq <> io.instruction

  val rowCnt  = Reg(UInt(16.W))
  val beatCnt = Reg(UInt(16.W))

  val addrReg = Reg(UInt(64.W))

  val input_axi_data_elements = config.axi_data_width / config.inputBus.elementBitWidth
  val kernel_axi_data_elements = config.axi_data_width / config.kernelBus.elementBitWidth
  val bias_axi_data_elements = config.axi_data_width / config.biasBus.elementBitWidth

  val inputBeatsPerRow =
    (config.inputBus.busWidth / config.axi_data_width)
  val kernelBeatsPerRow =
    (config.kernelBus.busWidth / config.axi_data_width)
  val biasBeatsPerRow =
    (config.biasBus.busWidth / config.axi_data_width)

  // Default outputs
  io.done := false.B
  inst.io.deq.ready := false.B
  when(state === sIdle){
    inst.io.deq.ready := true.B
  } 

  io.input_bus.req.valid  := false.B
  io.kernel_bus.req.valid := false.B
  io.bias_bus.req.valid   := false.B

  axi.ar.valid := false.B
  axi.ar.bits.addr  := 0.U
  axi.ar.bits.len   := 0.U
  axi.ar.bits.size  := log2Ceil(config.axi_data_width / 8).U
  axi.ar.bits.burst := 1.U // INCR

  axi.r.ready := false.B

  val inputBeatMask = ((1 << input_axi_data_elements) - 1).U(input_axi_data_elements.W)
  val kernelBeatMask = ((1 << kernel_axi_data_elements) - 1).U(kernel_axi_data_elements.W)
  val biasBeatMask = ((1 << bias_axi_data_elements) - 1).U(bias_axi_data_elements.W)

  val inputPerBeatMasks = Seq.tabulate(inputBeatsPerRow)(i => (inputBeatMask << (i * input_axi_data_elements)).asUInt.pad(config.inputBus.elements))
  val kernelPerBeatMasks = Seq.tabulate(kernelBeatsPerRow)(i => (kernelBeatMask << (i * kernel_axi_data_elements)).asUInt.pad(config.kernelBus.elements))
  val biasPerBeatMasks = Seq.tabulate(biasBeatsPerRow)(i => (biasBeatMask << (i * bias_axi_data_elements)).asUInt.pad(config.biasBus.elements))

  val inputWriteMask = Mux1H(
    UIntToOH(beatCnt, inputBeatsPerRow).asBools,
    inputPerBeatMasks
  )

  val kernelWriteMask = Mux1H(
    UIntToOH(beatCnt, kernelBeatsPerRow).asBools,
    kernelPerBeatMasks
  )

  val biasWriteMask = Mux1H(
    UIntToOH(beatCnt, biasBeatsPerRow).asBools,
    biasPerBeatMasks
  )

  val inputWriteData = Mux1H(
    UIntToOH(beatCnt, inputBeatsPerRow).asBools,
    Seq.tabulate(inputBeatsPerRow)(i => (axi.r.bits.data << (i * config.axi_data_width)).asUInt.pad(config.inputBus.busWidth))
  )

  val kernelWriteData = Mux1H(
    UIntToOH(beatCnt, kernelBeatsPerRow).asBools,
    Seq.tabulate(kernelBeatsPerRow)(i => (axi.r.bits.data << (i * config.axi_data_width)).asUInt.pad(config.kernelBus.busWidth))
  )

  val biasWriteData = Mux1H(
    UIntToOH(beatCnt, biasBeatsPerRow).asBools,
    Seq.tabulate(biasBeatsPerRow)(i => (axi.r.bits.data << (i * config.axi_data_width)).asUInt.pad(config.biasBus.busWidth))
  )

  io.input_bus.req.bits.addr := inst.io.deq.bits.scratchpad_addr + rowCnt
  io.input_bus.req.bits.data := inputWriteData
  io.input_bus.req.bits.mask := inputWriteMask
  io.kernel_bus.req.bits.addr := inst.io.deq.bits.scratchpad_addr + rowCnt
  io.kernel_bus.req.bits.data := kernelWriteData
  io.kernel_bus.req.bits.mask := kernelWriteMask
  io.bias_bus.req.bits.addr := inst.io.deq.bits.scratchpad_addr + rowCnt
  io.bias_bus.req.bits.data := biasWriteData
  io.bias_bus.req.bits.mask := biasWriteMask

  // ------------------------------
  // FSM
  // ------------------------------
  switch(state) {

    //----------------------------------------
    // Idle: wait for instruction
    //----------------------------------------
    is(sIdle) {
      when(io.instruction.fire) {
        addrReg := io.instruction.bits.external_addr
        rowCnt  := 0.U
        beatCnt := 0.U
        state   := sSendAR
      }
    }

    //----------------------------------------
    // Send AR request for a row
    //----------------------------------------
    is(sSendAR) {
      axi.ar.valid := true.B
      axi.ar.bits.addr  := addrReg
      when(inst.io.deq.bits.tensorType === TensorTypeHW.Input) {
        axi.ar.bits.len   := (inputBeatsPerRow - 1).U
      }
      .elsewhen(inst.io.deq.bits.tensorType === TensorTypeHW.Kernel) {
        axi.ar.bits.len   := (kernelBeatsPerRow - 1).U
      }
      .otherwise {
        axi.ar.bits.len   := (biasBeatsPerRow - 1).U
      }

      when(axi.ar.valid && axi.ar.ready) {
        beatCnt := 0.U
        state := sReadData
      }
    }

    //----------------------------------------
    // Read data beats
    //----------------------------------------
    is(sReadData) {

      when(axi.r.valid) {
        axi.r.ready := true.B

        when(inst.io.deq.bits.tensorType === TensorTypeHW.Input) {
          io.input_bus.req.valid := true.B
        }.elsewhen(inst.io.deq.bits.tensorType === TensorTypeHW.Kernel) {
          io.kernel_bus.req.valid := true.B
        }.otherwise {
          io.bias_bus.req.valid := true.B
        }

        beatCnt := beatCnt + 1.U

        when(axi.r.bits.last) {
          state := sNextRow
        }
      }
    }

    //----------------------------------------
    // go to next row or finish
    //----------------------------------------
    is(sNextRow) {
      rowCnt := rowCnt + 1.U
      addrReg := addrReg + inst.io.deq.bits.stride

      when(rowCnt === inst.io.deq.bits.rows - 1.U) {
        state := sIdle
        io.done := true.B
      }.otherwise {
        state := sSendAR
      }
    }
  }
}
