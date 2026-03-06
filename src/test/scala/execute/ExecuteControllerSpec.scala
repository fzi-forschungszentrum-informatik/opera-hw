/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.test

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import opera.{OPERAConfigs, OPERAConfig}
import opera.scratchpad.{ScratchpadReadIO, ScratchpadWriteIO, SRAMControllerIO}
import opera.types.{DataflowTypeHW, DataflowType}
import opera.execute.{BaseComputationUnit, ComputationUnitConfig, ExecuteController} 
import opera.execute.computation_units.{WeightStationarySystolicArray} 
import opera.instructions.ExecuteInstruction

class ExecuteControllerSpec extends AnyFlatSpec with CustomChiselSim with Matchers {
  
  behavior of "ExecuteController"

  class MockSRAMController[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T,U,V]) extends Module {

    val io = IO(new SRAMControllerIO(
        config.inputBus,
        config.kernelBus,
        config.biasBus,
        config.outputBus
    ))

    // Not used io's
    io.load_input.req.ready := true.B
    io.load_kernel.req.ready := true.B
    io.load_bias.req.ready := true.B
    io.store.req.ready := true.B
    io.store.resp.valid := false.B
    io.store.resp.bits.data := 0.U

    // Request always are ready to receive 
    io.exec_input.req.ready := true.B
    io.exec_kernel.req.ready := true.B
    io.exec_bias.req.ready := true.B
    io.exec_output.req.ready := true.B

    // Responses come one clock cycle after the actual request
    val input_req_valid = RegInit(false.B)
    val kernel_req_valid = RegInit(false.B)
    val bias_req_valid = RegInit(false.B)

    input_req_valid := io.exec_input.req.fire
    kernel_req_valid := io.exec_kernel.req.fire
    bias_req_valid := io.exec_bias.req.fire

    io.exec_input.resp.valid := input_req_valid
    io.exec_input.resp.bits.data := 5.U
    io.exec_kernel.resp.valid := kernel_req_valid
    io.exec_kernel.resp.bits.data := 3.U
    io.exec_bias.resp.valid := bias_req_valid
    io.exec_bias.resp.bits.data := 2.U
  } 
  
  // Create a mock config
  val mockConfig = OPERAConfigs.defaultConfig.copy(
    cuFactory = (config: ComputationUnitConfig[SInt, SInt, SInt]) => new WeightStationarySystolicArray[SInt, SInt, SInt](config),
    cu_config = ComputationUnitConfig(
        N = 16,
        K = 16,
        ADataType = SInt(8.W),
        BDataType = SInt(8.W),
        DDataType = SInt(32.W),
        //supportedDataflows = Seq(DataflowType.IS, DataflowType.WS, DataflowType.OS)
        supportedDataflows = Seq(DataflowType.WS)
    )
  )

  class MockExecuteControllerWithSRAM[T <: Data, U <: Data, V <: Data](config: OPERAConfig[T, U, V]) extends Module{
    val io = IO(
        Flipped(Decoupled(new ExecuteInstruction(mockConfig.inputBus.totalRows, mockConfig.kernelBus.totalRows, mockConfig.biasBus.totalRows, mockConfig.outputBus.totalRows, mockConfig.max_rows_per_load_store)))
    )

    class InternalIOs extends Bundle{
        val input_bus_req_fire = Bool()
        val kernel_bus_req_fire = Bool()
        val bias_bus_req_fire = Bool()
        val output_bus_req_fire = Bool()

        val input_bus_resp_fire = Bool()
        val kernel_bus_resp_fire = Bool()
        val bias_bus_resp_fire = Bool()
    } 

    val internal_io = IO(
        new InternalIOs
    )

    val execute_controller = Module(new ExecuteController(config))
    val sram_controller = Module(new MockSRAMController(config))

    sram_controller.io.exec_input <> execute_controller.io.input_bus
    sram_controller.io.exec_kernel <> execute_controller.io.kernel_bus
    sram_controller.io.exec_bias <> execute_controller.io.bias_bus
    sram_controller.io.exec_output <> execute_controller.io.output_bus

    internal_io.input_bus_req_fire := execute_controller.io.input_bus.req.fire
    internal_io.kernel_bus_req_fire := execute_controller.io.kernel_bus.req.fire
    internal_io.bias_bus_req_fire := execute_controller.io.bias_bus.req.fire
    internal_io.output_bus_req_fire := execute_controller.io.output_bus.req.fire

    internal_io.input_bus_resp_fire := execute_controller.io.input_bus.resp.fire
    internal_io.kernel_bus_resp_fire := execute_controller.io.kernel_bus.resp.fire
    internal_io.bias_bus_resp_fire := execute_controller.io.bias_bus.resp.fire

    // Not used IO's
    sram_controller.io.load_input.req.valid := false.B
    sram_controller.io.load_input.req.bits.addr := 0.U
    sram_controller.io.load_input.req.bits.data := 0.U
    sram_controller.io.load_input.req.bits.mask := 0.U
    sram_controller.io.load_kernel.req.valid := false.B
    sram_controller.io.load_kernel.req.bits.addr := 0.U
    sram_controller.io.load_kernel.req.bits.data := 0.U
    sram_controller.io.load_kernel.req.bits.mask := 0.U
    sram_controller.io.load_bias.req.valid := false.B
    sram_controller.io.load_bias.req.bits.addr := 0.U
    sram_controller.io.load_bias.req.bits.data := 0.U
    sram_controller.io.load_bias.req.bits.mask := 0.U
    sram_controller.io.store.req.valid := false.B
    sram_controller.io.store.req.bits.addr := 0.U
    sram_controller.io.store.resp.ready := false.B

    io <> execute_controller.io.instruction
  } 
  
  it should "accept instruction when idle" in {
    simulate(new ExecuteController(mockConfig)) { dut =>
      dut.io.instruction.valid.poke(false.B)
      dut.clock.step(1)
      
      // Controller should be ready in idle state
      dut.io.instruction.ready.expect(true.B)
      
      // Provide an instruction
      dut.io.instruction.valid.poke(true.B)
      dut.io.instruction.bits.input_scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.kernel_scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.bias_scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.output_scratchpad_addr.poke(0.U)
      dut.io.instruction.bits.preload_rows.poke(4.U)
      dut.io.instruction.bits.feeding_rows.poke(8.U)
      dut.io.instruction.bits.expected_output_rows.poke(8.U)
      dut.io.instruction.bits.dataflow.poke(DataflowTypeHW.IS)
      dut.io.instruction.bits.preload.poke(true.B)
      
      dut.clock.step(1)
      
      // Should no longer be ready after accepting instruction
      dut.io.instruction.ready.expect(false.B)
    }
  }
  
  mockConfig.cu_config.supportedDataflows.foreach(dataflow => {
    it should s"perform ${dataflow} dataflow with preload and feeding phases" in {
        simulate(new MockExecuteControllerWithSRAM(mockConfig)) { dut =>        

        val PRELOAD_ROWS = 16
        val FEED_ROWS = 16
        val EXPECTED_OUTPUT_ROWS = 16

        // Reset
        dut.reset.poke(true.B)
        dut.io.valid.poke(false.B)
        dut.clock.step(32)
        dut.reset.poke(false.B)
        dut.clock.step(1)
        
        // Submit instruction with preload
        dut.io.valid.poke(true.B)
        dut.io.bits.input_scratchpad_addr.poke(0.U)
        dut.io.bits.kernel_scratchpad_addr.poke(32.U)
        dut.io.bits.bias_scratchpad_addr.poke(0.U)
        dut.io.bits.output_scratchpad_addr.poke(32.U)
        dut.io.bits.preload_rows.poke(PRELOAD_ROWS.U)
        dut.io.bits.feeding_rows.poke(FEED_ROWS.U)
        dut.io.bits.expected_output_rows.poke(EXPECTED_OUTPUT_ROWS.U)
        dut.io.bits.dataflow.poke(DataflowType.toHW(dataflow))
        dut.io.bits.preload.poke(true.B)
        
        dut.clock.step(1)
        dut.io.valid.poke(false.B)

        var preloadRequestCounter = 0
        var preloadResponseCounter = 0
        var feedingRequestInputCounter = 0
        var feedingResponseInputCounter = 0
        var feedingRequestKernelCounter = 0
        var feedingResponseKernelCounter = 0
        var feedingRequestBiasCounter = 0
        var feedingResponseBiasCounter = 0

        var outputRequestCounter = 0
        
        for (_ <- 0 until 200) {

            require(preloadRequestCounter <= PRELOAD_ROWS, s"No more than ${PRELOAD_ROWS} rows should be requested during preload!")
            require(preloadResponseCounter <= PRELOAD_ROWS, s"No more than ${PRELOAD_ROWS} rows should be responded during preload!")
            require(feedingRequestInputCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be requested during feed!")
            require(feedingResponseInputCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be responded during feed!")
            require(feedingRequestKernelCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be requested during feed!")
            require(feedingResponseKernelCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be responded during feed!")
            require(feedingRequestBiasCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be requested during feed!")
            require(feedingResponseBiasCounter <= FEED_ROWS, s"No more than ${FEED_ROWS} rows should be responded during feed!")
            require(outputRequestCounter <= EXPECTED_OUTPUT_ROWS, s"No more than ${EXPECTED_OUTPUT_ROWS} rows should be requested for output!")

            if(dut.internal_io.output_bus_req_fire.peekBoolean()){
                outputRequestCounter += 1
            } 

            if(preloadResponseCounter < PRELOAD_ROWS){ 
                // Check that during the preload phase, the correct amount of data is requested and also the correct amount of responses arrive
                if(dataflow == DataflowType.IS){
                    if(dut.internal_io.input_bus_req_fire.peekBoolean()){
                        preloadRequestCounter += 1
                    } 
                    if(dut.internal_io.input_bus_resp_fire.peekBoolean()){
                        preloadResponseCounter += 1
                    } 
                } 
                else if (dataflow == DataflowType.WS){
                    if(dut.internal_io.kernel_bus_req_fire.peekBoolean()){
                        preloadRequestCounter += 1
                    } 
                    if(dut.internal_io.kernel_bus_resp_fire.peekBoolean()){
                        preloadResponseCounter += 1
                    } 
                } 
                else if (dataflow == DataflowType.OS){
                    if(dut.internal_io.bias_bus_req_fire.peekBoolean()){
                        preloadRequestCounter += 1
                    } 
                    if(dut.internal_io.bias_bus_resp_fire.peekBoolean()){
                        preloadResponseCounter += 1
                    } 
                } 
            } 
            else if(feedingResponseInputCounter < FEED_ROWS && feedingResponseKernelCounter < FEED_ROWS && feedingResponseBiasCounter < FEED_ROWS){
                // During feeding phase, check correct amount of data is requested on the other two buses
                if(dut.internal_io.input_bus_req_fire.peekBoolean()){
                    feedingRequestInputCounter += 1
                } 
                if(dut.internal_io.input_bus_resp_fire.peekBoolean()){
                    feedingResponseInputCounter += 1
                } 
                if(dut.internal_io.kernel_bus_req_fire.peekBoolean()){
                    feedingRequestKernelCounter += 1
                } 
                if(dut.internal_io.kernel_bus_resp_fire.peekBoolean()){
                    feedingResponseKernelCounter += 1
                } 
                if(dut.internal_io.bias_bus_req_fire.peekBoolean()){
                    feedingRequestBiasCounter += 1
                } 
                if(dut.internal_io.bias_bus_resp_fire.peekBoolean()){
                    feedingResponseBiasCounter += 1
                } 
                if (dataflow == DataflowType.IS){
                    require(dut.internal_io.input_bus_req_fire.peekBoolean() == false, "During feed, with dataflow IS, input_bus should never request data!")
                } 
                else if (dataflow == DataflowType.WS){
                    require(dut.internal_io.kernel_bus_req_fire.peekBoolean() == false, "During feed, with dataflow WS, kernel_bus should never request data!")
                } 
                else if (dataflow == DataflowType.OS){
                    require(dut.internal_io.bias_bus_req_fire.peekBoolean() == false, "During feed, with dataflow OS, bias_bus should never request data!")
                } 
            } 
            else{
                // Waiting for output state, no input should be activated in this period
                require(dut.internal_io.input_bus_req_fire.peekBoolean() == false, "While waiting for the output to finish, no request should be done in input_bus!")
                require(dut.internal_io.kernel_bus_req_fire.peekBoolean() == false, "While waiting for the output to finish, no request should be done in kernel_bus!")
                require(dut.internal_io.bias_bus_req_fire.peekBoolean() == false, "While waiting for the output to finish, no request should be done in bias_bus!")
                require(dut.internal_io.input_bus_resp_fire.peekBoolean() == false, "While waiting for the output to finish, no response should be received in input_bus!")
                require(dut.internal_io.kernel_bus_resp_fire.peekBoolean() == false, "While waiting for the output to finish, no response should be received in kernel_bus!")
                require(dut.internal_io.bias_bus_resp_fire.peekBoolean() == false, "While waiting for the output to finish, no response should be received in bias_bus!")
            } 

            dut.clock.step(1)
        } 
        }
    }
  }) 
}