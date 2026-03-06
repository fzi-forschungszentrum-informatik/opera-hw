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
import opera.types.{TensorTypeHW, DataflowTypeHW} 
import opera.instructions.ReservationStation

class ReservationStationSpec extends AnyFlatSpec with CustomChiselSim with Matchers {
    behavior of "ReservationStation"

    it should s"dispatch load/exec/store respecting dependencies and busy flags" in {
        simulate(new ReservationStation(OPERAConfigs.defaultConfig)) { dut =>

            // Initialize
            dut.io.load_in.valid.poke(false)
            dut.io.execute_in.valid.poke(false)
            dut.io.store_in.valid.poke(false)

            dut.io.load_out.ready.poke(true)
            dut.io.execute_out.ready.poke(true)
            dut.io.store_out.ready.poke(true)

            dut.io.load_done.poke(false)
            dut.io.execute_done.poke(false)
            dut.io.store_done.poke(false)

            dut.clock.step()

            // -------------------------
            // 1. ENQUEUE A LOAD
            // -------------------------
            dut.io.load_in.valid.poke(true)
            dut.io.load_in.bits.scratchpad_addr.poke(3)
            dut.io.load_in.bits.external_addr.poke(100)
            dut.io.load_in.bits.rows.poke(1)
            dut.io.load_in.bits.stride.poke(1)
            dut.io.load_in.bits.tensorType.poke(TensorTypeHW.Input)

            while (!dut.io.load_in.ready.peekBoolean()) { dut.clock.step() }
            dut.clock.step()
            dut.io.load_in.valid.poke(false)

            // Load should dispatch immediately
            dut.io.load_out.valid.expect(true)
            dut.io.load_out.bits.scratchpad_addr.expect(3)

            // Make load busy
            dut.clock.step()
            dut.io.load_done.poke(false)

            // -------------------------
            // 2. ENQUEUE AN EXECUTE Dependent on LOAD
            // -------------------------
            dut.io.execute_in.valid.poke(true)
            dut.io.execute_in.bits.input_scratchpad_addr.poke(3)  // depends on load
            dut.io.execute_in.bits.kernel_scratchpad_addr.poke(0)
            dut.io.execute_in.bits.bias_scratchpad_addr.poke(0)
            dut.io.execute_in.bits.output_scratchpad_addr.poke(7)
            dut.io.execute_in.bits.preload_rows.poke(0)
            dut.io.execute_in.bits.feeding_rows.poke(0)
            dut.io.execute_in.bits.expected_output_rows.poke(1)
            dut.io.execute_in.bits.dataflow.poke(DataflowTypeHW.IS)
            dut.io.execute_in.bits.preload.poke(false)

            while (!dut.io.execute_in.ready.peekBoolean()) { dut.clock.step() }
            dut.clock.step()
            dut.io.execute_in.valid.poke(false)

            // EXECUTE must *NOT* dispatch yet — load not finished
            dut.io.execute_out.valid.expect(false)

            // -------------------------
            // 3. COMPLETE LOAD
            // -------------------------
            dut.io.load_done.poke(true)
            dut.clock.step()
            dut.io.load_done.poke(false)

            // Now execute should dispatch
            dut.io.execute_out.valid.expect(true)
            dut.io.execute_out.bits.input_scratchpad_addr.expect(3)

            dut.clock.step()

            // -------------------------
            // 4. ENQUEUE A STORE Dependent on EXEC
            // -------------------------
            dut.io.store_in.valid.poke(true)
            dut.io.store_in.bits.scratchpad_addr.poke(7)  // depends on execute output
            dut.io.store_in.bits.external_addr.poke(200)
            dut.io.store_in.bits.rows.poke(1)
            dut.io.store_in.bits.stride.poke(1)

            while (!dut.io.store_in.ready.peekBoolean()) { dut.clock.step() }
            dut.clock.step()
            dut.io.store_in.valid.poke(false)

            // STORE must NOT dispatch yet (execute still busy)
            dut.io.store_out.valid.expect(false)

            // -------------------------
            // 5. COMPLETE EXECUTE
            // -------------------------
            dut.io.execute_done.poke(true)
            dut.clock.step()
            dut.io.execute_done.poke(false)

            // STORE should now dispatch
            dut.io.store_out.valid.expect(true)
            dut.io.store_out.bits.scratchpad_addr.expect(7)

            dut.clock.step()

            // -------------------------
            // 6. COMPLETE STORE
            // -------------------------
            dut.io.store_done.poke(true)
            dut.clock.step()
            dut.io.store_done.poke(false)

            // No more outputs should be valid
            dut.io.load_out.valid.expect(false)
            dut.io.execute_out.valid.expect(false)
            dut.io.store_out.valid.expect(false)
        }
    }

    it should s"handle multiple outstanding entries per queue with correct order and dependency behavior" in {
        simulate(new ReservationStation(OPERAConfigs.defaultConfig)) { dut =>

            // ----------
            // Initialize
            // ----------
            dut.io.load_in.valid.poke(false)
            dut.io.execute_in.valid.poke(false)
            dut.io.store_in.valid.poke(false)

            dut.io.load_out.ready.poke(true)
            dut.io.execute_out.ready.poke(true)
            dut.io.store_out.ready.poke(true)

            dut.io.load_done.poke(false)
            dut.io.execute_done.poke(false)
            dut.io.store_done.poke(false)

            dut.clock.step()

            // ================================================================
            // 1. ENQUEUE MULTIPLE LOADS
            // ================================================================
            // Load0 writes addr = 10
            // Load1 writes addr = 11
            // Load2 writes addr = 12

            val loadAddrs = Seq(10, 11, 12)

            for (addr <- loadAddrs) {
                dut.io.load_in.valid.poke(true)
                dut.io.load_in.bits.scratchpad_addr.poke(addr)
                dut.io.load_in.bits.external_addr.poke(100)
                dut.io.load_in.bits.rows.poke(1)
                dut.io.load_in.bits.stride.poke(1)
                dut.io.load_in.bits.tensorType.poke(TensorTypeHW.Input)

                while (!dut.io.load_in.ready.peekBoolean()) { dut.clock.step() }
                dut.clock.step()
                dut.io.load_in.valid.poke(false)

                if(dut.io.load_out.valid.peekBoolean()){
                    dut.io.load_out.bits.scratchpad_addr.expect(loadAddrs(0))
                } 
            }

            // ================================================================
            // LOAD0 should dispatch immediately and block loads 1 & 2
            // ================================================================            

            // Make LOAD0 busy
            dut.clock.step()

            // Enqueue two executes that depend on LOAD1 and LOAD2
            // EXEC0 depends on scratchpad 11 (Load1)
            // EXEC1 depends on scratchpad 12 (Load2)

            def pushExec(inAddr: Int, outAddr: Int): Unit = {
                dut.io.execute_in.valid.poke(true)
                dut.io.execute_in.bits.input_scratchpad_addr.poke(inAddr)
                dut.io.execute_in.bits.kernel_scratchpad_addr.poke(0)
                dut.io.execute_in.bits.bias_scratchpad_addr.poke(0)
                dut.io.execute_in.bits.output_scratchpad_addr.poke(outAddr)
                dut.io.execute_in.bits.preload_rows.poke(0)
                dut.io.execute_in.bits.feeding_rows.poke(0)
                dut.io.execute_in.bits.expected_output_rows.poke(1)
                dut.io.execute_in.bits.dataflow.poke(DataflowTypeHW.IS)
                dut.io.execute_in.bits.preload.poke(false)

                while (!dut.io.execute_in.ready.peekBoolean()) { dut.clock.step() }
                dut.clock.step()
                dut.io.execute_in.valid.poke(false)
            }

            pushExec(11, 20) // EXEC0
            pushExec(12, 21) // EXEC1

            // ================================================================
            // LOAD1 and LOAD2 must not dispatch until load_done is signaled
            // ================================================================

            // Complete LOAD0
            dut.io.load_done.poke(true)
            dut.clock.step()
            dut.io.load_done.poke(false)

            // LOAD1 must now dispatch
            dut.io.load_out.valid.expect(true)
            dut.io.load_out.bits.scratchpad_addr.expect(loadAddrs(1))
            dut.clock.step()

            // LOAD1 is busy
            dut.io.load_out.valid.expect(false)

            // Complete LOAD1
            dut.io.load_done.poke(true)
            dut.clock.step()
            dut.io.load_done.poke(false)
            // Once this completes, exec0 (depends on addr 11) should be dispatched
            dut.io.execute_out.valid.expect(true)
            dut.io.execute_out.bits.input_scratchpad_addr.expect(11)

            // LOAD2 must now dispatch
            dut.io.load_out.valid.expect(true)
            dut.io.load_out.bits.scratchpad_addr.expect(loadAddrs(2))
            dut.clock.step()

            // ================================================================
            // After LOAD2 completes, EXEC1 (depends on addr 12) should dispatch
            // ================================================================
            dut.io.load_done.poke(true)

            dut.clock.step()

            dut.io.load_done.poke(false)
            
            dut.clock.step()

            // EXEC is busy now
            dut.io.execute_out.valid.expect(false)

            dut.clock.step()

            // Complete EXEC0
            dut.io.execute_done.poke(true)
            dut.clock.step()
            dut.io.execute_done.poke(false)

            // EXEC1 should now dispatch
            dut.io.execute_out.valid.expect(true)
            dut.io.execute_out.bits.input_scratchpad_addr.expect(12)
            dut.clock.step()

            // ================================================================
            // FINAL: Ensure queues are empty and nothing more dispatches
            // ================================================================
            dut.io.execute_done.poke(true)
            dut.clock.step()
            dut.io.execute_done.poke(false)

            dut.io.load_out.valid.expect(false)
            dut.io.execute_out.valid.expect(false)
            dut.io.store_out.valid.expect(false)
        }
    }
} 