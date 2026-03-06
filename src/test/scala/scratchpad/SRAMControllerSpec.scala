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
import opera.types.TensorType
import opera.scratchpad.{SRAMController, ScratchpadReadIO, ScratchpadWriteIO} 
import opera.OPERAConfigs

class SRAMControllerSpec extends AnyFlatSpec with CustomChiselSim with Matchers {
  
  behavior of "SRAMController"
  
  it should "write and read back data correctly on all buses" in {
    val mockConfig = OPERAConfigs.defaultConfig
    
    simulate(new SRAMController(mockConfig)) { dut =>

        dut.reset.poke(true.B)
        dut.clock.step(10)
        dut.reset.poke(false.B)
        dut.clock.step()

        val buses = Map(
            dut.io.load_input -> dut.io.exec_input,
            dut.io.load_kernel -> dut.io.exec_kernel,
            dut.io.load_bias -> dut.io.exec_bias,
            dut.io.exec_output -> dut.io.store
        )

        var address = 0
        var data = 0xCAFE

        for ((write_bus: ScratchpadWriteIO, read_bus: ScratchpadReadIO) <- buses) {

            write_bus.req.bits.addr.poke(address.U)
            write_bus.req.bits.data.poke(data.U)
            write_bus.req.bits.mask.poke(0xFFFF.U)
            write_bus.req.valid.poke(true.B)
            dut.clock.step()

            // Wait for write request to be accepted
            while (!write_bus.req.ready.peekBoolean()) {
                dut.clock.step()
            }
            write_bus.req.valid.poke(false.B)
            dut.clock.step()

            // Issue a request to read that same address
            read_bus.req.bits.addr.poke(address.U)
            read_bus.req.valid.poke(true.B)
            dut.clock.step()

            // Wait for read request to be accepted
            while (!read_bus.req.ready.peekBoolean()) {
                dut.clock.step()
            }
            read_bus.req.valid.poke(false.B)
            dut.clock.step()

            // Wait for read response to become valid
            while (!read_bus.resp.valid.peekBoolean()) {
                dut.clock.step()
            }

            // Compare against the expected data (the one we just wrote)
            read_bus.resp.bits.data.expect(data.U)
            read_bus.resp.ready.poke(true.B)
            dut.clock.step()

            // Finish test, move to next pair of buses
            read_bus.resp.ready.poke(false.B)
            data += 1
            address += 1
        }
    } 
  } 

  it should "write and read back data correctly (simultaneous requests)" in {
    val mockConfig = OPERAConfigs.defaultConfig

    simulate(new SRAMController(mockConfig)) { dut =>
        enableWaves()

        dut.reset.poke(true.B)
        dut.clock.step(10)
        dut.reset.poke(false.B)
        dut.clock.step()

        val buses = List(
            (dut.io.load_input, dut.io.load_kernel, dut.io.exec_input, dut.io.exec_kernel),
            (dut.io.load_bias, dut.io.exec_output, dut.io.exec_bias, dut.io.store)
        )

        var address = 0
        var data = 0xCAFE

        for ((write_bus_0: ScratchpadWriteIO, write_bus_1: ScratchpadWriteIO, read_bus_0: ScratchpadReadIO, read_bus_1: ScratchpadReadIO) <- buses) {

            write_bus_0.req.bits.addr.poke(address.U)
            write_bus_0.req.bits.data.poke(data.U)
            write_bus_0.req.bits.mask.poke(0xFFFF.U)
            write_bus_0.req.valid.poke(true.B)

            write_bus_1.req.bits.addr.poke((address + 1).U)
            write_bus_1.req.bits.data.poke((data + 1).U)
            write_bus_1.req.bits.mask.poke(0xFFFF.U)
            write_bus_1.req.valid.poke(true.B)

            dut.clock.step()

            var write_bus_0_accepted = false
            var write_bus_1_accepted = false

            // Wait for both writes to be accepted
            for (_ <- 0 until 200) {

                if(write_bus_0.req.ready.peekBoolean()){
                    write_bus_0_accepted = true
                    write_bus_0.req.valid.poke(false.B)
                    dut.clock.step()
                } 

                if(write_bus_1.req.ready.peekBoolean()){
                    write_bus_1_accepted = true
                    write_bus_1.req.valid.poke(false.B)
                    dut.clock.step()
                } 

                dut.clock.step()
            } 

            require(write_bus_0_accepted, "Write request on bus 0 was not accepted")
            require(write_bus_1_accepted, "Write request on bus 1 was not accepted")

            // Now, generate simultaneous read requests
            read_bus_0.req.bits.addr.poke(address.U)
            read_bus_0.req.valid.poke(true.B)
            read_bus_1.req.bits.addr.poke((address + 1).U)
            read_bus_1.req.valid.poke(true.B)
            dut.clock.step()

            var read_bus_0_accepted = false
            var read_bus_1_accepted = false

            // Wait for both read requests to be accepted
            for (_ <- 0 until 200) {

                if(read_bus_0.req.ready.peekBoolean()){
                    read_bus_0_accepted = true
                    read_bus_0.req.valid.poke(false.B)
                    dut.clock.step()
                } 

                if(read_bus_1.req.ready.peekBoolean()){
                    read_bus_1_accepted = true
                    read_bus_1.req.valid.poke(false.B)
                    dut.clock.step()
                } 

                dut.clock.step()
            } 

            require(read_bus_0_accepted, "Read request on bus 0 was not accepted")
            require(read_bus_1_accepted, "Read request on bus 1 was not accepted")

            // Wait for read responses to become valid
            var read_bus_0_response = false
            var read_bus_1_response = false

            for (_ <- 0 until 200){

                if(read_bus_0.resp.valid.peekBoolean()){
                    read_bus_0.resp.bits.data.expect(data.U)
                    read_bus_0.resp.ready.poke(true.B)
                    read_bus_0_response = true
                    dut.clock.step()
                } 

                if(read_bus_1.resp.valid.peekBoolean()){
                    read_bus_1.resp.bits.data.expect((data + 1).U)
                    read_bus_1.resp.ready.poke(true.B)
                    read_bus_1_response = true
                    dut.clock.step()
                } 

                if(read_bus_0_response){
                    read_bus_0.resp.ready.poke(false.B)
                } 

                if(read_bus_1_response){
                    read_bus_1.resp.ready.poke(false.B)
                } 
                
                dut.clock.step()
            } 

            require(read_bus_0_response, "Read response on bus 0 was not received")
            require(read_bus_1_response, "Read response on bus 0 was not received")

            dut.clock.step()

            address += 16
            data += 16
        } 
    } 
  } 
}