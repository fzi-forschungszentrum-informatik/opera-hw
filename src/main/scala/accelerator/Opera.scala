/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera

import chisel3._
import chisel3.util._
import opera.scratchpad.{SRAMController}
import opera.execute.{ExecuteController} 
import opera.load.{LoadController} 
import opera.store.{StoreController} 
import opera.instructions.{ReservationStation, LoadInstruction, StoreInstruction, ExecuteInstruction} 
import opera.axi.{SimpleAXI4ReadIO, SimpleAXI4WriteIO} 
import opera.{OPERAConfig, BusInfo}

class OPERAIO(
    inputBus: BusInfo,
    kernelBus: BusInfo,
    biasBus: BusInfo,
    outputBus: BusInfo,
    max_sram_rows: Int,
    max_rows_per_load_store: Int,
    axi_addr_width: Int,
    axi_data_width: Int
) extends Bundle{
    // Reservation station IO's
    val load_in = Flipped(Decoupled(new LoadInstruction(max_sram_rows, max_rows_per_load_store)))
    val store_in = Flipped(Decoupled(new StoreInstruction(outputBus.totalRows, max_rows_per_load_store)))
    val execute_in = Flipped(Decoupled(new ExecuteInstruction(inputBus.totalRows, kernelBus.totalRows, biasBus.totalRows, outputBus.totalRows, max_rows_per_load_store)))

    val load_empty    = Output(Bool())
    val execute_empty = Output(Bool())
    val store_empty   = Output(Bool())
    val all_empty     = Output(Bool())

    // AXI interfaces
    val load_axi = new SimpleAXI4ReadIO(axi_addr_width, axi_data_width)
    val store_axi = new SimpleAXI4WriteIO(axi_addr_width, axi_data_width)
} 

class OPERA[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T,U,V]) extends Module {

    val io = IO(new OPERAIO(
      config.inputBus,
      config.kernelBus,
      config.biasBus,
      config.outputBus,
      config.max_sram_rows,
      config.max_rows_per_load_store,
      config.axi_addr_width,
      config.axi_data_width
    ))

    // SRAMController contains the actual SRAMs
    val sram_controller = Module(new SRAMController(config))

    // Controllers
    val execute_controller = Module(new ExecuteController(config))
    val load_controller = Module(new LoadController(config))
    val store_controller = Module(new StoreController(config))

    // Reservation station
    val reservation_station = Module(new ReservationStation(config))

    // Connect load, store and execute controllers to SRAMs
    sram_controller.io.exec_input <> execute_controller.io.input_bus
    sram_controller.io.exec_kernel <> execute_controller.io.kernel_bus
    sram_controller.io.exec_bias <> execute_controller.io.bias_bus
    sram_controller.io.exec_output <> execute_controller.io.output_bus
    sram_controller.io.load_input <> load_controller.io.input_bus
    sram_controller.io.load_kernel <> load_controller.io.kernel_bus
    sram_controller.io.load_bias <> load_controller.io.bias_bus
    sram_controller.io.store <> store_controller.io.output_bus

    // Connect reservation station to controllers
    execute_controller.io.instruction <> reservation_station.io.execute_out
    load_controller.io.instruction <> reservation_station.io.load_out
    store_controller.io.instruction <> reservation_station.io.store_out
    reservation_station.io.load_done := load_controller.io.done
    reservation_station.io.store_done := store_controller.io.done
    reservation_station.io.execute_done := execute_controller.io.done

    // Connect OPERA IO's
    io.load_in <> reservation_station.io.load_in
    io.store_in <> reservation_station.io.store_in
    io.execute_in <> reservation_station.io.execute_in
    io.load_empty := reservation_station.io.load_empty
    io.store_empty := reservation_station.io.store_empty
    io.execute_empty := reservation_station.io.execute_empty
    io.all_empty := reservation_station.io.all_empty
    io.load_axi <> load_controller.axi
    io.store_axi <> store_controller.axi
} 