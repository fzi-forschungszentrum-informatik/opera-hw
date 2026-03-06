/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.instructions

import chisel3._
import chisel3.util._
import opera.{OPERAConfig, BusInfo}
import opera.types.TensorType

class ReservationStationIO(
    inputBus: BusInfo,
    kernelBus: BusInfo,
    biasBus: BusInfo,
    outputBus: BusInfo,
    max_sram_rows: Int,
    max_rows_per_load_store: Int,
) extends Bundle{
    val load_in = Flipped(Decoupled(new LoadInstruction(max_sram_rows, max_rows_per_load_store)))
    val store_in = Flipped(Decoupled(new StoreInstruction(outputBus.totalRows, max_rows_per_load_store)))
    val execute_in = Flipped(Decoupled(new ExecuteInstruction(inputBus.totalRows, kernelBus.totalRows, biasBus.totalRows, outputBus.totalRows, max_rows_per_load_store)))

    val load_out = Decoupled(new LoadInstruction(max_sram_rows, max_rows_per_load_store))
    val store_out = Decoupled(new StoreInstruction(outputBus.totalRows, max_rows_per_load_store))
    val execute_out = Decoupled(new ExecuteInstruction(inputBus.totalRows, kernelBus.totalRows, biasBus.totalRows, outputBus.totalRows, max_rows_per_load_store))

    // TODO: could/should be replaced with tagged versions, in case we allow multiple in-flight requests.
    val load_done    = Input(Bool())
    val store_done   = Input(Bool())
    val execute_done = Input(Bool())

    val load_empty    = Output(Bool())
    val execute_empty = Output(Bool())
    val store_empty   = Output(Bool())
    val all_empty     = Output(Bool())
} 

class ReservationStation[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T, U, V]) extends Module {
  val io = IO(new ReservationStationIO(
    config.inputBus,
    config.kernelBus,
    config.biasBus,
    config.outputBus,
    config.max_sram_rows,
    config.max_rows_per_load_store
  ))

  // --- Configuration: queue depths (tune as needed) ---
  val LOAD_DEPTH = 8
  val STORE_DEPTH = 8
  val EXEC_DEPTH = 8

  // --- Internal circular FIFOs (simple regs) ---
  // Load queue
  val loadEntries = RegInit(VecInit(Seq.fill(LOAD_DEPTH)(0.U.asTypeOf(io.load_in.bits))))
  val loadValid = RegInit(VecInit(Seq.fill(LOAD_DEPTH)(false.B)))
  val loadWritePtr = RegInit(0.U(log2Ceil(LOAD_DEPTH).W))
  val loadReadPtr  = RegInit(0.U(log2Ceil(LOAD_DEPTH).W))
  val loadCount = RegInit(0.U((log2Ceil(LOAD_DEPTH) + 1).W))

  // Store queue
  val storeEntries = RegInit(VecInit(Seq.fill(STORE_DEPTH)(0.U.asTypeOf(io.store_in.bits))))
  val storeValid = RegInit(VecInit(Seq.fill(STORE_DEPTH)(false.B)))
  val storeWritePtr = RegInit(0.U(log2Ceil(STORE_DEPTH).W))
  val storeReadPtr  = RegInit(0.U(log2Ceil(STORE_DEPTH).W))
  val storeCount = RegInit(0.U((log2Ceil(STORE_DEPTH) + 1).W))

  // Execute queue
  val execEntries = RegInit(VecInit(Seq.fill(EXEC_DEPTH)(0.U.asTypeOf(io.execute_in.bits))))
  val execValid = RegInit(VecInit(Seq.fill(EXEC_DEPTH)(false.B)))
  val execWritePtr = RegInit(0.U(log2Ceil(EXEC_DEPTH).W))
  val execReadPtr  = RegInit(0.U(log2Ceil(EXEC_DEPTH).W))
  val execCount = RegInit(0.U((log2Ceil(EXEC_DEPTH) + 1).W))

  // Busy flags: set when a head instruction has been accepted (issued) to the controller,
  // cleared when controller asserts the corresponding done signal.
  val loadBusy = RegInit(false.B)
  val execBusy = RegInit(false.B)
  val storeBusy = RegInit(false.B)

  // helper functions for pointer increments
  def incPtr(ptr: UInt, depth: Int): UInt = {
    val next = ptr + 1.U
    Mux(next === depth.U, 0.U, next)
  }

  // --- Enqueue logic ---
  // load_in
  val loadCanEnq = io.load_in.valid && (loadCount =/= LOAD_DEPTH.U)
  io.load_in.ready := (loadCount =/= LOAD_DEPTH.U)
  when (loadCanEnq) {
    loadEntries(loadWritePtr) := io.load_in.bits
    loadValid(loadWritePtr) := true.B
    loadWritePtr := incPtr(loadWritePtr, LOAD_DEPTH)
    loadCount := loadCount + 1.U
  }

  // store_in
  val storeCanEnq = io.store_in.valid && (storeCount =/= STORE_DEPTH.U)
  io.store_in.ready := (storeCount =/= STORE_DEPTH.U)
  when (storeCanEnq) {
    storeEntries(storeWritePtr) := io.store_in.bits
    storeValid(storeWritePtr) := true.B
    storeWritePtr := incPtr(storeWritePtr, STORE_DEPTH)
    storeCount := storeCount + 1.U
  }

  // execute_in
  val execCanEnq = io.execute_in.valid && (execCount =/= EXEC_DEPTH.U)
  io.execute_in.ready := (execCount =/= EXEC_DEPTH.U)
  when (execCanEnq) {
    execEntries(execWritePtr) := io.execute_in.bits
    execValid(execWritePtr) := true.B
    execWritePtr := incPtr(execWritePtr, EXEC_DEPTH)
    execCount := execCount + 1.U
  }

  // --- Head pointers / head entries (only valid if count > 0) ---
  val loadHeadValid = loadCount =/= 0.U && loadValid(loadReadPtr)
  val storeHeadValid = storeCount =/= 0.U && storeValid(storeReadPtr)
  val execHeadValid = execCount =/= 0.U && execValid(execReadPtr)

  val loadHead = loadEntries(loadReadPtr)
  val storeHead = storeEntries(storeReadPtr)
  val execHead = execEntries(execReadPtr)

  // --- Dependency checks ---
  // Execute depends on loads that write input/kernel/bias addresses: check entire Load queue for any valid entry writing to these addresses.
  // TODO: Careful with this, because what if there are loads for the next matrix multiplication that are already in the queue?
  def anyLoadMatches(addr: UInt, tensorType: TensorType): Bool = {
    val matches = Wire(Vec(LOAD_DEPTH, Bool()))
    for (i <- 0 until LOAD_DEPTH) {
      matches(i) := loadValid(i) && (loadEntries(i).scratchpad_addr === addr) && (loadEntries(i).tensorType === TensorType.toHW(tensorType))
    }
    matches.asUInt.orR
  }

  // Store depends on executes that will write to the store's scratchpad addr: check entire Exec queue for any valid entry with output_scratchpad_addr == store.scratchpad_addr
  def anyExecWrites(addr: UInt): Bool = {
    val matches = Wire(Vec(EXEC_DEPTH, Bool()))
    for (i <- 0 until EXEC_DEPTH) {
      matches(i) := execValid(i) && (execEntries(i).output_scratchpad_addr === addr)
    }
    matches.asUInt.orR
  }

  // Execute is also blocked by pending Executes that will produce required inputs
  def anyExecProduces(addr: UInt): Bool = {
    val matches = Wire(Vec(EXEC_DEPTH, Bool()))
    for (i <- 0 until EXEC_DEPTH) {
      matches(i) := execValid(i) && (execEntries(i).output_scratchpad_addr === addr) && !(i.U === execReadPtr)
    }
    matches.asUInt.orR
  }

  // For an execute head, check if any load in the load queue produces data it needs
  val execBlockedByLoad = Wire(Bool())
  execBlockedByLoad := false.B
  when (execHeadValid) {
    val e = execHead
    val inMatch = anyLoadMatches(e.input_scratchpad_addr, TensorType.Input)
    val kerMatch = anyLoadMatches(e.kernel_scratchpad_addr, TensorType.Kernel)
    val biasMatch = anyLoadMatches(e.bias_scratchpad_addr, TensorType.Bias)
    execBlockedByLoad := inMatch || kerMatch || biasMatch
  }

  // For an execute head, check if any exec in the exec queue produces data it needs
  val execBlockedByExec = Wire(Bool())
  execBlockedByExec := false.B
  when (execHeadValid) {
    val e = execHead
    val inMatch = anyExecProduces(e.input_scratchpad_addr)
    val kerMatch = anyExecProduces(e.kernel_scratchpad_addr)
    val biasMatch = anyExecProduces(e.bias_scratchpad_addr)
    execBlockedByExec := inMatch || kerMatch || biasMatch
  }

  // For a store head, check if any execute in the exec queue produces the data it needs (i.e., output_scratchpad_addr == store.scratchpad_addr)
  val storeBlockedByExec = Wire(Bool())
  storeBlockedByExec := false.B
  when (storeHeadValid) {
    storeBlockedByExec := anyExecWrites(storeHead.scratchpad_addr)
  }

  // --- Dispatch / Issue logic ---
  // load_out, store_out, execute_out are produced by this module
  io.load_out.valid := false.B
  io.load_out.bits := 0.U.asTypeOf(io.load_out.bits)
  io.store_out.valid := false.B
  io.store_out.bits := 0.U.asTypeOf(io.store_out.bits)
  io.execute_out.valid := false.B
  io.execute_out.bits := 0.U.asTypeOf(io.execute_out.bits)

  // LOAD issue: can issue (transfer) if head valid, not busy, and consumer ready
  val loadAccepted = Wire(Bool())
  loadAccepted := false.B
  when (loadHeadValid && !loadBusy) {
    io.load_out.valid := true.B
    io.load_out.bits := loadHead
    when (io.load_out.ready) {
      // transfer occurs: mark unit busy. Do NOT dequeue until load_done asserted.
      loadAccepted := true.B
    }
  }

  // EXECUTE issue: need head valid, not busy, consumer ready, and not blocked by pending loads/execs
  val execAccepted = Wire(Bool())
  execAccepted := false.B
  when (execHeadValid && !execBusy && !execBlockedByLoad && !execBlockedByExec) {
    io.execute_out.valid := true.B
    io.execute_out.bits := execHead
    when (io.execute_out.ready) {
      execAccepted := true.B
    }
  }

  // STORE issue: need head valid, not busy, consumer ready, and not blocked by pending execs
  val storeAccepted = Wire(Bool())
  storeAccepted := false.B
  when (storeHeadValid && !storeBusy && !storeBlockedByExec) {
    io.store_out.valid := true.B
    io.store_out.bits := storeHead
    when (io.store_out.ready) {
      storeAccepted := true.B
    }
  }

  // When accepted (transfer), set busy flags. We keep the queue entry valid until the corresponding done signal arrives.
  when (loadAccepted) {
    loadBusy := true.B
  }
  when (execAccepted) {
    execBusy := true.B
  }
  when (storeAccepted) {
    storeBusy := true.B
  }

  // Completion handling: when functional unit asserts done, clear busy and dequeue the head entry.
  // Note: this simplistic approach assumes the done corresponds to the head that was issued.
  when (io.load_done) {
    // clear busy (if any) and remove head entry if it was valid
    loadBusy := false.B
    when (loadHeadValid) {
      loadValid(loadReadPtr) := false.B
      loadReadPtr := incPtr(loadReadPtr, LOAD_DEPTH)
      loadCount := loadCount - 1.U
    }
  }

  when (io.execute_done) {
    execBusy := false.B
    when (execHeadValid) {
      execValid(execReadPtr) := false.B
      execReadPtr := incPtr(execReadPtr, EXEC_DEPTH)
      execCount := execCount - 1.U
    }
  }

  when (io.store_done) {
    storeBusy := false.B
    when (storeHeadValid) {
      storeValid(storeReadPtr) := false.B
      storeReadPtr := incPtr(storeReadPtr, STORE_DEPTH)
      storeCount := storeCount - 1.U
    }
  }

  // Queue empty conditions (no valid entries)
  val loadQEmpty    = !loadValid.reduce(_ || _)
  val executeQEmpty = !execValid.reduce(_ || _)
  val storeQEmpty   = !storeValid.reduce(_ || _)

  // Consider busy flags: an instruction is still "pending"
  // if the queue is empty BUT the controller is still processing
  io.load_empty    := loadQEmpty    && !loadBusy
  io.execute_empty := executeQEmpty && !execBusy
  io.store_empty   := storeQEmpty   && !storeBusy

  // Global empty: ALL queues empty AND no instructions in flight
  io.all_empty := io.load_empty && io.execute_empty && io.store_empty
}