/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.execute

import chisel3._
import chisel3.util._
import opera.{OPERAConfig, BusInfo} 
import opera.scratchpad.{ScratchpadReadIO, ScratchpadWriteIO} 
import opera.types.{DataflowTypeHW, DataflowType} 
import opera.instructions.ExecuteInstruction

class ExecuteControllerIO(
    inputBus: BusInfo,
    kernelBus: BusInfo,
    biasBus: BusInfo,
    outputBus: BusInfo,
    max_rows_per_load_store: Int
    ) extends Bundle {

    // Connection to the scratchpads
    val input_bus = new ScratchpadReadIO(inputBus)
    val kernel_bus = new ScratchpadReadIO(kernelBus)
    val bias_bus = new ScratchpadReadIO(biasBus)
    val output_bus = new ScratchpadWriteIO(outputBus)

    // Instruction to execute
    val instruction = Flipped(Decoupled(new ExecuteInstruction(inputBus.totalRows, kernelBus.totalRows, biasBus.totalRows, outputBus.totalRows, max_rows_per_load_store)))

    val done = Output(Bool())
} 

class ExecuteController[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T, U, V]) extends Module {
  val io = IO(new ExecuteControllerIO(
    config.inputBus,
    config.kernelBus,
    config.biasBus,
    config.outputBus,
    config.max_rows_per_load_store
  ))

  // Instantiate computation unit
  val computation_unit = Module(config.cuFactory(config.cu_config))

  val has_IS = config.cu_config.hasDataflow(DataflowType.IS)
  val has_WS = config.cu_config.hasDataflow(DataflowType.WS)
  val has_OS = config.cu_config.hasDataflow(DataflowType.OS)

  // Instruction register
  val inst = Module(new Queue(io.instruction.bits.cloneType, entries=1))

  inst.io.enq <> io.instruction

  // State machine
  val sIdle :: sPreload :: sWaitPreload :: sFeeding :: sWaitingForOutputs :: Nil = Enum(5)
  val state = RegInit(sIdle)

  // Per-channel request & response counters
  val reqCntA = RegInit(0.U(16.W))   // input requests sent
  val reqCntB = RegInit(0.U(16.W))   // kernel requests sent
  val reqCntD = RegInit(0.U(16.W))   // bias requests sent

  val preloadCntA = RegInit(0.U(16.W))  // input responses received
  val preloadCntB = RegInit(0.U(16.W))  // kernel responses received
  val preloadCntD = RegInit(0.U(16.W))  // bias responses received

  val fedCntA = RegInit(0.U(16.W))  // input responses fed to computation unit
  val fedCntB = RegInit(0.U(16.W))  // kernel responses fed to computation unit
  val fedCntD = RegInit(0.U(16.W))  // bias responses fed to computation unit

  // Output counter
  val outputRowCnt = RegInit(0.U(16.W))

  // Default instruction handshake
  io.done := false.B
  inst.io.deq.ready := false.B
  when(state === sIdle){
    inst.io.deq.ready := true.B
  } 

  // Determine which channels participate in preload (stationary) and feeding (streaming)
  val preload_input  = ((state === sPreload) || (state === sWaitPreload)) && (inst.io.deq.bits.dataflow === DataflowTypeHW.IS) && has_IS
  val preload_kernel = ((state === sPreload) || (state === sWaitPreload)) && (inst.io.deq.bits.dataflow === DataflowTypeHW.WS) && has_WS
  val preload_bias   = ((state === sPreload) || (state === sWaitPreload)) && (inst.io.deq.bits.dataflow === DataflowTypeHW.OS) && has_OS

  val feed_input  = (state === sFeeding) && (inst.io.deq.bits.dataflow =/= DataflowTypeHW.IS)  // needs streamed inputs
  val feed_kernel = (state === sFeeding) && (inst.io.deq.bits.dataflow =/= DataflowTypeHW.WS)
  val feed_bias   = (state === sFeeding) && (inst.io.deq.bits.dataflow =/= DataflowTypeHW.OS)

  // ----- Per-channel small response FIFOs -----
  // store the raw UInts from scratchpad responses; later convert to CU types with asTypeOf()
  val fifoA = Module(new Queue(UInt(io.input_bus.resp.bits.data.getWidth.W), entries = 4)).suggestName("fifoA")
  val fifoB = Module(new Queue(UInt(io.kernel_bus.resp.bits.data.getWidth.W), entries = 4)).suggestName("fifoB")
  val fifoD = Module(new Queue(UInt(io.bias_bus.resp.bits.data.getWidth.W), entries = 4)).suggestName("fifoD")

  // Connect scratchpad resp -> fifos
  fifoA.io.enq.valid := io.input_bus.resp.valid
  fifoA.io.enq.bits  := io.input_bus.resp.bits.data
  io.input_bus.resp.ready := fifoA.io.enq.ready

  fifoB.io.enq.valid := io.kernel_bus.resp.valid
  fifoB.io.enq.bits  := io.kernel_bus.resp.bits.data
  io.kernel_bus.resp.ready := fifoB.io.enq.ready

  fifoD.io.enq.valid := io.bias_bus.resp.valid
  fifoD.io.enq.bits  := io.bias_bus.resp.bits.data
  io.bias_bus.resp.ready := fifoD.io.enq.ready

  // Count amount of preloaded data
  when(computation_unit.io.in_a.valid && preload_input) { preloadCntA := preloadCntA + 1.U }
  when(computation_unit.io.in_b.valid && preload_kernel) { preloadCntB := preloadCntB + 1.U }
  when(computation_unit.io.in_d.valid && preload_bias) { preloadCntD := preloadCntD + 1.U }

  // Count amount of data fed to the computation unit
  when(fifoA.io.deq.fire) { fedCntA := fedCntA + 1.U }
  when(fifoB.io.deq.fire) { fedCntB := fedCntB + 1.U }
  when(fifoD.io.deq.fire) { fedCntD := fedCntD + 1.U }

  // ----- Requests: issue independently per channel -----
  // decide whether we should be issuing a request this cycle for each channel
  val wantPreloadA = preload_input && (reqCntA < inst.io.deq.bits.preload_rows)
  val wantPreloadB = preload_kernel && (reqCntB < inst.io.deq.bits.preload_rows)
  val wantPreloadD = preload_bias && (reqCntD < inst.io.deq.bits.preload_rows)

  val wantFeedA = feed_input && (reqCntA < inst.io.deq.bits.feeding_rows)
  val wantFeedB = feed_kernel && (reqCntB < inst.io.deq.bits.feeding_rows)
  val wantFeedD = feed_bias && (reqCntD < inst.io.deq.bits.feeding_rows)

  // Drive the scratchpad request valids independently
  // TODO: Maybe we can overlap the scratchpad reads? So that we are already requesting data for the feed buses while we are still preloading?
  io.input_bus.req.valid  := wantPreloadA || wantFeedA
  io.kernel_bus.req.valid := wantPreloadB || wantFeedB
  io.bias_bus.req.valid   := wantPreloadD || wantFeedD

  // Addresses use per-channel request counters (streams address per request)
  io.input_bus.req.bits.addr  := inst.io.deq.bits.input_scratchpad_addr  + reqCntA
  io.kernel_bus.req.bits.addr := inst.io.deq.bits.kernel_scratchpad_addr + reqCntB
  io.bias_bus.req.bits.addr   := inst.io.deq.bits.bias_scratchpad_addr   + reqCntD

  // Increment per-channel request counters only when their req actually fires
  when(io.input_bus.req.fire)  { reqCntA := reqCntA + 1.U }
  when(io.kernel_bus.req.fire) { reqCntB := reqCntB + 1.U }
  when(io.bias_bus.req.fire)   { reqCntD := reqCntD + 1.U }

  // ----- Output connection as before (CU -> output scratchpad) -----
  io.output_bus.req.valid := computation_unit.io.out_c.valid
  computation_unit.io.out_c.ready := io.output_bus.req.ready
  io.output_bus.req.bits.data := computation_unit.io.out_c.bits.data.asTypeOf(io.output_bus.req.bits.data)
  io.output_bus.req.bits.addr := inst.io.deq.bits.output_scratchpad_addr + outputRowCnt
  io.output_bus.req.bits.mask := Fill(io.output_bus.req.bits.mask.getWidth, 1.U)

  when(io.output_bus.req.fire) { outputRowCnt := outputRowCnt + 1.U }

  // ----- Preload behaviour: while in preload state, feed stationary values into CU -----
  // We will drive CU inputs from the FIFOs in preload to let the CU latch stationary values.
  // Only channels participating in preload will be driven.

  computation_unit.io.in_a.bits.data := fifoA.io.deq.bits.asTypeOf(computation_unit.io.in_a.bits.data)
  computation_unit.io.in_b.bits.data := fifoB.io.deq.bits.asTypeOf(computation_unit.io.in_b.bits.data)
  computation_unit.io.in_d.bits.data := fifoD.io.deq.bits.asTypeOf(computation_unit.io.in_d.bits.data)

  // Stationary bit: tells CU that this input is stationary (preload)
  computation_unit.io.in_a.bits.stationary := preload_input && fifoA.io.deq.valid
  computation_unit.io.in_b.bits.stationary := preload_kernel && fifoB.io.deq.valid
  computation_unit.io.in_d.bits.stationary := preload_bias && fifoD.io.deq.valid

  // ----- Feeding stage row-joiner -----
  // Determine which channels must supply streaming data during feeding
  // For channels that are not streaming (stationary), we don't require FIFO data.

  // For row readiness during feed, require FIFOs to have valid data for the channels that are needed.
  val feedRowReady = Wire(Bool())
  when(feed_kernel && feed_bias && fifoB.io.deq.valid && fifoD.io.deq.valid){
    // IS
    feedRowReady := true.B
  } 
  .elsewhen(feed_input && feed_bias && fifoA.io.deq.valid && fifoD.io.deq.valid){
    // WS
    feedRowReady := true.B
  } 
  .elsewhen(feed_input && feed_kernel && fifoA.io.deq.valid && fifoB.io.deq.valid){
    // OS
    feedRowReady := true.B
  } 
  .otherwise{
    feedRowReady := false.B
  } 

  fifoA.io.deq.ready := computation_unit.io.in_a.fire
  fifoB.io.deq.ready := computation_unit.io.in_b.fire
  fifoD.io.deq.ready := computation_unit.io.in_d.fire

  val input_preload_valid = preload_input && fifoA.io.deq.valid
  val kernel_preload_valid = preload_kernel && fifoB.io.deq.valid
  val bias_preload_valid = preload_bias && fifoD.io.deq.valid

  val input_feed_valid = ((state === sFeeding && feedRowReady) && feed_input)
  val kernel_feed_valid = ((state === sFeeding && feedRowReady) && feed_kernel)
  val bias_feed_valid = ((state === sFeeding && feedRowReady) && feed_bias)

  computation_unit.io.in_a.valid := input_preload_valid || input_feed_valid
  computation_unit.io.in_b.valid := kernel_preload_valid || kernel_feed_valid
  computation_unit.io.in_d.valid := bias_preload_valid || bias_feed_valid

  // ---- State transitions and resets ----

  // Reset counters when new instruction fired
  when(inst.io.deq.valid && state === sIdle) {

    // reset counters
    reqCntA := 0.U
    reqCntB := 0.U
    reqCntD := 0.U

    preloadCntA := 0.U
    preloadCntB := 0.U
    preloadCntD := 0.U

    fedCntA := 0.U
    fedCntB := 0.U
    fedCntD := 0.U

    outputRowCnt := 0.U

    // choose initial state depending on preload flag
    when(inst.io.deq.bits.preload) {
      state := sPreload
    }.otherwise {
      state := sFeeding
    }
  }

  // Preload state: issue preload requests as long as any participating channel still needs to send preload rows
  when(state === sPreload) {
    // if all participating channels have issued all preload requests, move to waitPreload
    val doneA = !preload_input || (reqCntA === inst.io.deq.bits.preload_rows)
    val doneB = !preload_kernel || (reqCntB === inst.io.deq.bits.preload_rows)
    val doneD = !preload_bias || (reqCntD === inst.io.deq.bits.preload_rows)

    when(doneA && doneB && doneD) {
      state := sWaitPreload
    }
  }

  // WaitPreload: wait for all responses of preload to arrive (tracked by respCnt*)
  when(state === sWaitPreload) {
    val doneA = !preload_input || (preloadCntA >= inst.io.deq.bits.preload_rows)
    val doneB = !preload_kernel || (preloadCntB >= inst.io.deq.bits.preload_rows)
    val doneD = !preload_bias || (preloadCntD >= inst.io.deq.bits.preload_rows)

    when(doneA && doneB && doneD) {
      // All stationary data has been received; transition to feeding
      // Reset per-channel request counters so feeding uses fresh counts starting at 0
      reqCntA := 0.U
      reqCntB := 0.U
      reqCntD := 0.U
      // also reset response counters if desired (or leave for debug)
      preloadCntA := 0.U
      preloadCntB := 0.U
      preloadCntD := 0.U

      state := sFeeding
    }
  }

  // Feeding: issue streaming requests for channels that require streaming; finish when all requests for all channels done
  when(state === sFeeding) {
    val feeding_rows_threshold = inst.io.deq.bits.feeding_rows - 1.U
    val doneA = !feed_input || (fedCntA === feeding_rows_threshold)
    val doneB = !feed_kernel || (fedCntB === feeding_rows_threshold)
    val doneD = !feed_bias || (fedCntD === feeding_rows_threshold)

    when(doneA && doneB && doneD) {
      state := sWaitingForOutputs
    }
  }

  // Waiting for outputs: wait until all expected output rows are written
  when(state === sWaitingForOutputs) {
    when(outputRowCnt === (inst.io.deq.bits.expected_output_rows - 1.U)) {
      state := sIdle
      inst.io.deq.ready := true.B
      io.done := true.B
    }
  }
}
