/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.scratchpad

import chisel3._
import chisel3.util._
import scala.collection.mutable.ListBuffer
import opera.scratchpad.{SRAMBank}
import opera.{OPERAConfig, BusInfo} 
import opera.types.TensorType

class ScratchpadReadReq(val rows: Int) extends Bundle {
  val addr = UInt(log2Ceil(rows).W)
}

class TaggedScratchpadReadReq(val rows: Int, val masterIdWidth: Int) extends Bundle {
  val addr = UInt(log2Ceil(rows).W)
  val masterId = UInt(masterIdWidth.W)
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
}

class ScratchpadReadIO(val busInfo: BusInfo) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(busInfo.totalRows))
  val resp = Flipped(Decoupled(new ScratchpadReadResp(busInfo.busWidth)))
}

class ScratchpadWriteReq(val rows: Int, val w: Int, val rowSize: Int) extends Bundle {
  val addr = Output(UInt(log2Ceil(rows).W))
  val data = Output(UInt(w.W))
  val mask = Output(UInt(rowSize.W))
}

class ScratchpadWriteIO(val busInfo: BusInfo) extends Bundle {
    val req = Decoupled(new ScratchpadWriteReq(busInfo.totalRows, busInfo.busWidth, busInfo.elements))
} 

class SRAMControllerIO(
    inputBus: BusInfo,
    kernelBus: BusInfo,
    biasBus: BusInfo,
    outputBus: BusInfo
    ) extends Bundle {

    // Buses for execute module
    val exec_input = Flipped(new ScratchpadReadIO(inputBus))
    val exec_kernel = Flipped(new ScratchpadReadIO(kernelBus))
    val exec_bias = Flipped(new ScratchpadReadIO(biasBus))
    val exec_output = Flipped(new ScratchpadWriteIO(outputBus))

    // Buses for load module
    val load_input = Flipped(new ScratchpadWriteIO(inputBus))
    val load_kernel = Flipped(new ScratchpadWriteIO(kernelBus))
    val load_bias = Flipped(new ScratchpadWriteIO(biasBus))

    // Buses for store module
    val store = Flipped(new ScratchpadReadIO(outputBus))
} 

class SRAMController[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T,U,V]) extends Module {

    val io = IO(new SRAMControllerIO(
        config.inputBus,
        config.kernelBus,
        config.biasBus,
        config.outputBus
    ))

    val sram_banks = Module(new SRAMBank(config.sram_configs))

    // This is where the magic happens ;)
    config.sram_configs.zipWithIndex.map { case (sram_config, idx) =>
        val sram_io = sram_banks.io.srams(idx)

        // Get which buses can read from this sram
        val read_buses = ListBuffer[ScratchpadReadIO]()
        if(sram_config.tags.contains(TensorType.Input)) read_buses.append(io.exec_input)
        if(sram_config.tags.contains(TensorType.Kernel)) read_buses.append(io.exec_kernel)
        if(sram_config.tags.contains(TensorType.Bias)) read_buses.append(io.exec_bias)
        if(sram_config.tags.contains(TensorType.Output)) read_buses.append(io.store)

        val numReadMasters = read_buses.length

        if(numReadMasters > 1)
        {
            val read_arb = Module(new Arbiter(new TaggedScratchpadReadReq(sram_config.numRows, log2Ceil(numReadMasters)), numReadMasters))
            val respQueues = VecInit(Seq.fill(numReadMasters)(Module(new Queue(new ScratchpadReadResp(sram_config.rowSizeBits), 2)).io))

            read_buses.zipWithIndex.foreach({
                case (bus, idx) => 
                    val reqWithId = Wire(new TaggedScratchpadReadReq(sram_config.numRows, log2Ceil(numReadMasters)))
                    reqWithId.addr := bus.req.bits.addr
                    reqWithId.masterId := idx.U
                    read_arb.io.in(idx).bits := reqWithId
                    read_arb.io.in(idx).valid := bus.req.valid
                    bus.req.ready := read_arb.io.in(idx).ready

                    bus.resp <> respQueues(idx).deq
                })

            val read_arbiter_out = read_arb.io.out
            sram_io.readEnable := read_arbiter_out.valid
            read_arbiter_out.ready := true.B // Careful with this, is the SRAMBank really always ready?
            sram_io.readAddr := read_arbiter_out.bits.addr

            val id = RegNext(read_arbiter_out.bits.masterId)

            for (i <- 0 until numReadMasters) {
                respQueues(i).enq.valid := sram_io.readValid && (id === i.U)
                respQueues(i).enq.bits.data := sram_io.readData.asTypeOf(respQueues(i).enq.bits.data)
            }
        } 
        else{
            val read_bus = read_buses(0)

            sram_io.readEnable := read_bus.req.valid
            read_bus.req.ready := sram_io.readValid
            sram_io.readAddr := read_bus.req.bits.addr

            read_bus.resp.valid := sram_io.readValid
            read_bus.resp.bits.data := sram_io.readData
        } 

        // Get which buses can write from this sram
        val write_buses = ListBuffer[ScratchpadWriteIO]()
        if(sram_config.tags.contains(TensorType.Input)) write_buses.append(io.load_input)
        if(sram_config.tags.contains(TensorType.Kernel)) write_buses.append(io.load_kernel)
        if(sram_config.tags.contains(TensorType.Bias)) write_buses.append(io.load_bias)
        if(sram_config.tags.contains(TensorType.Output)) write_buses.append(io.exec_output)

        val numWriteMasters = write_buses.length

        if(numWriteMasters > 1)
        {
            val write_arb = Module(new Arbiter(new ScratchpadWriteReq(sram_config.numRows, sram_config.rowSizeBits, sram_config.rowSize), write_buses.length))
            write_buses.zipWithIndex.foreach({case (bus, idx) => 
                write_arb.io.in(idx).valid := bus.req.valid
                write_arb.io.in(idx).bits.addr := bus.req.bits.addr
                write_arb.io.in(idx).bits.data := bus.req.bits.data
                write_arb.io.in(idx).bits.mask := bus.req.bits.mask
                bus.req.ready := write_arb.io.in(idx).ready
            })

            val write_arbiter_out = write_arb.io.out

            sram_io.writeEnable := write_arbiter_out.valid
            sram_io.writeAddr := write_arbiter_out.bits.addr
            sram_io.writeData := write_arbiter_out.bits.data.asTypeOf(sram_io.writeData)
            sram_io.writeMask := write_arbiter_out.bits.mask.asTypeOf(sram_io.writeMask)
            write_arbiter_out.ready := true.B // TODO: check if it is safe to assume this
        } 
        else{
            val write_bus = write_buses(0)

            sram_io.writeEnable := write_bus.req.valid
            sram_io.writeAddr := write_bus.req.bits.addr
            sram_io.writeData := write_bus.req.bits.data.asTypeOf(sram_io.writeData)
            sram_io.writeMask := write_bus.req.bits.mask.asTypeOf(sram_io.writeMask)
            write_bus.req.ready := true.B // TODO: check if it is safe to assume this
        } 
        
    }

} 