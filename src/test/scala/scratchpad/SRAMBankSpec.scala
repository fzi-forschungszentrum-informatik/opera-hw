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
import opera.scratchpad.{SRAMBank, TwoPortSRAM, SRAMConfig} 

class SRAMBankSpec extends AnyFlatSpec with CustomChiselSim with Matchers {
  
  behavior of "TwoPortSRAM"
  
  it should "write and read back data correctly" in {
    val config = SRAMConfig(
      numRows = 16,
      rowSize = 4,
      dataType = UInt(8.W),
      tags = Seq(TensorType.Input)
    )
    
    simulate(new TwoPortSRAM(config)) { dut =>
      // Write data to address 5
      dut.io.writeEnable.poke(true.B)
      dut.io.writeAddr.poke(5.U)
      dut.io.writeMask.poke(0xF.U.asTypeOf(dut.io.writeMask))
      dut.io.writeData(0).poke(0xAA.U)
      dut.io.writeData(1).poke(0xBB.U)
      dut.io.writeData(2).poke(0xCC.U)
      dut.io.writeData(3).poke(0xDD.U)
      dut.io.readEnable.poke(false.B)
      dut.clock.step()
      
      // Disable write
      dut.io.writeEnable.poke(false.B)
      
      // Read from address 5
      dut.io.readEnable.poke(true.B)
      dut.io.readAddr.poke(5.U)
      dut.clock.step()
      
      // Check readValid is asserted
      dut.io.readValid.expect(true.B)
      
      // Verify read data
      dut.io.readData(0).expect(0xAA.U)
      dut.io.readData(1).expect(0xBB.U)
      dut.io.readData(2).expect(0xCC.U)
      dut.io.readData(3).expect(0xDD.U)
    }
  }
  
  it should "handle simultaneous read and write to different addresses" in {
    val config = SRAMConfig(
      numRows = 16,
      rowSize = 2,
      dataType = UInt(16.W),
      tags = Seq(TensorType.Kernel)
    )
    
    simulate(new TwoPortSRAM(config)) { dut =>
      // Initialize address 3 with data
      dut.io.writeEnable.poke(true.B)
      dut.io.writeAddr.poke(3.U)
      dut.io.writeMask.poke(0x3.U.asTypeOf(dut.io.writeMask))
      dut.io.writeData(0).poke(0x1234.U)
      dut.io.writeData(1).poke(0x5678.U)
      dut.io.readEnable.poke(false.B)
      dut.clock.step()
      
      // Simultaneously write to address 7 and read from address 3
      dut.io.writeEnable.poke(true.B)
      dut.io.writeAddr.poke(7.U)
      dut.io.writeData(0).poke(0xABCD.U)
      dut.io.writeData(1).poke(0xEF01.U)
      
      dut.io.readEnable.poke(true.B)
      dut.io.readAddr.poke(3.U)
      dut.clock.step()
      
      // Verify read from address 3
      dut.io.readValid.expect(true.B)
      dut.io.readData(0).expect(0x1234.U)
      dut.io.readData(1).expect(0x5678.U)
      
      // Now read from address 7 to verify the write
      dut.io.writeEnable.poke(false.B)
      dut.io.readAddr.poke(7.U)
      dut.clock.step()
      
      dut.io.readValid.expect(true.B)
      dut.io.readData(0).expect(0xABCD.U)
      dut.io.readData(1).expect(0xEF01.U)
    }
  }
  
  it should "correctly handle readValid signal" in {
    val config = SRAMConfig(
      numRows = 8,
      rowSize = 1,
      dataType = UInt(32.W),
      tags = Seq(TensorType.Input)
    )
    
    simulate(new TwoPortSRAM(config)) { dut =>
      // Initially readValid should be false
      dut.io.readEnable.poke(false.B)
      dut.io.writeEnable.poke(false.B)
      dut.io.writeMask.poke(true.B.asTypeOf(dut.io.writeMask))
      dut.clock.step()
      dut.io.readValid.expect(false.B)
      
      // Enable read
      dut.io.readEnable.poke(true.B)
      dut.io.readAddr.poke(0.U)
      dut.clock.step()
      
      // ReadValid should now be true
      dut.io.readValid.expect(true.B)
      
      // Disable read
      dut.io.readEnable.poke(false.B)
      dut.clock.step()
      
      // ReadValid should now be false
      dut.io.readValid.expect(false.B)
    }
  }
  
  behavior of "SRAMBank"
  
  it should "correctly instantiate multiple SRAMs with different configurations" in {
    val configs = Seq(
      SRAMConfig(numRows = 8, rowSize = 2, dataType = UInt(8.W), tags = Seq(TensorType.Input)),
      SRAMConfig(numRows = 16, rowSize = 4, dataType = UInt(16.W), tags = Seq(TensorType.Kernel)),
      SRAMConfig(numRows = 4, rowSize = 8, dataType = UInt(32.W), tags = Seq(TensorType.Bias))
    )
    
    simulate(new SRAMBank(configs)) { dut =>
      // Test SRAM 0 (8x2, 8-bit)
      dut.io.srams(0).writeEnable.poke(true.B)
      dut.io.srams(0).writeAddr.poke(2.U)
      dut.io.srams(0).writeMask.poke(0x3.U.asTypeOf(dut.io.srams(0).writeMask))
      dut.io.srams(0).writeData(0).poke(0x12.U)
      dut.io.srams(0).writeData(1).poke(0x34.U)
      dut.io.srams(0).readEnable.poke(false.B)
      dut.clock.step()
      
      dut.io.srams(0).writeEnable.poke(false.B)
      dut.io.srams(0).readEnable.poke(true.B)
      dut.io.srams(0).readAddr.poke(2.U)
      dut.clock.step()
      
      dut.io.srams(0).readValid.expect(true.B)
      dut.io.srams(0).readData(0).expect(0x12.U)
      dut.io.srams(0).readData(1).expect(0x34.U)
      
      // Test SRAM 1 (16x4, 16-bit)
      dut.io.srams(1).writeEnable.poke(true.B)
      dut.io.srams(1).writeAddr.poke(10.U)
      dut.io.srams(1).writeMask.poke(0xF.U.asTypeOf(dut.io.srams(1).writeMask))
      dut.io.srams(1).writeData(0).poke(0x1111.U)
      dut.io.srams(1).writeData(1).poke(0x2222.U)
      dut.io.srams(1).writeData(2).poke(0x3333.U)
      dut.io.srams(1).writeData(3).poke(0x4444.U)
      dut.io.srams(1).readEnable.poke(false.B)
      dut.clock.step()
      
      dut.io.srams(1).writeEnable.poke(false.B)
      dut.io.srams(1).readEnable.poke(true.B)
      dut.io.srams(1).readAddr.poke(10.U)
      dut.clock.step()
      
      dut.io.srams(1).readValid.expect(true.B)
      dut.io.srams(1).readData(0).expect(0x1111.U)
      dut.io.srams(1).readData(1).expect(0x2222.U)
      dut.io.srams(1).readData(2).expect(0x3333.U)
      dut.io.srams(1).readData(3).expect(0x4444.U)
      
      // Test SRAM 2 (4x8, 32-bit)
      dut.io.srams(2).writeEnable.poke(true.B)
      dut.io.srams(2).writeAddr.poke(1.U)
      dut.io.srams(2).writeMask.poke(0xFF.U.asTypeOf(dut.io.srams(2).writeMask))
      for (i <- 0 until 8) {
        dut.io.srams(2).writeData(i).poke((0x10000000 + i * 0x1000000).U)
      }
      dut.io.srams(2).readEnable.poke(false.B)
      dut.clock.step()
      
      dut.io.srams(2).writeEnable.poke(false.B)
      dut.io.srams(2).readEnable.poke(true.B)
      dut.io.srams(2).readAddr.poke(1.U)
      dut.clock.step()
      
      dut.io.srams(2).readValid.expect(true.B)
      for (i <- 0 until 8) {
        dut.io.srams(2).readData(i).expect((0x10000000 + i * 0x1000000).U)
      }
    }
  }
  
  it should "operate all SRAMs independently" in {
    val configs = Seq(
      SRAMConfig(numRows = 4, rowSize = 2, dataType = UInt(8.W), tags = Seq(TensorType.Input)),
      SRAMConfig(numRows = 4, rowSize = 2, dataType = UInt(8.W), tags = Seq(TensorType.Kernel))
    )
    
    simulate(new SRAMBank(configs)) { dut =>
      // Write to both SRAMs simultaneously
      dut.io.srams(0).writeEnable.poke(true.B)
      dut.io.srams(0).writeAddr.poke(1.U)
      dut.io.srams(0).writeMask.poke(0x3.U.asTypeOf(dut.io.srams(0).writeMask))
      dut.io.srams(0).writeData(0).poke(0xAA.U)
      dut.io.srams(0).writeData(1).poke(0xBB.U)
      
      dut.io.srams(1).writeEnable.poke(true.B)
      dut.io.srams(1).writeAddr.poke(2.U)
      dut.io.srams(1).writeMask.poke(0x3.U.asTypeOf(dut.io.srams(1).writeMask))
      dut.io.srams(1).writeData(0).poke(0xCC.U)
      dut.io.srams(1).writeData(1).poke(0xDD.U)
      
      dut.clock.step()
      
      // Read from both SRAMs simultaneously
      dut.io.srams(0).writeEnable.poke(false.B)
      dut.io.srams(0).readEnable.poke(true.B)
      dut.io.srams(0).readAddr.poke(1.U)
      
      dut.io.srams(1).writeEnable.poke(false.B)
      dut.io.srams(1).readEnable.poke(true.B)
      dut.io.srams(1).readAddr.poke(2.U)
      
      dut.clock.step()
      
      // Verify both reads
      dut.io.srams(0).readValid.expect(true.B)
      dut.io.srams(0).readData(0).expect(0xAA.U)
      dut.io.srams(0).readData(1).expect(0xBB.U)
      
      dut.io.srams(1).readValid.expect(true.B)
      dut.io.srams(1).readData(0).expect(0xCC.U)
      dut.io.srams(1).readData(1).expect(0xDD.U)
    }
  }
}