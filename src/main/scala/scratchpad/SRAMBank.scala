/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.scratchpad

import chisel3._
import chisel3.util._
import opera.types.TensorType

// Configuration for a single SRAM
case class SRAMConfig[T <: Data](
  numRows: Int,
  rowSize: Int,
  dataType: T,
  tags: Seq[TensorType] 
) {
  require(numRows > 0, "Number of rows must be positive")
  require(rowSize > 0, "Row size must be positive")

  val rowSizeBits = rowSize * dataType.getWidth // Size of one row in bits
  val sizeBytes = (numRows * rowSizeBits / 8).toInt // Size of the entire scratchpad in bytes
  val sizeElements = (numRows * rowSize).toInt // Size of the entire scratchpad in elements of type dataType
}

object SRAMConfig {
  // Helper function to calculate number of rows from size in KB
  def calculateNumRows[T <: Data](
    sizeKB: Double,
    rowSize: Int,
    dataType: T
  ): Int = {
    require(sizeKB > 0, "Size in KB must be positive")
    require(rowSize > 0, "Row size must be positive")
    require(dataType.getWidth > 0, "Data type must have a valid width")
    
    val sizeInBits = sizeKB * 1024 * 8
    val bitsPerRow = rowSize * dataType.getWidth
    val numRows = (sizeInBits / bitsPerRow).toInt
    
    require(numRows > 0, s"Calculated number of rows must be positive. " +
      s"Size: ${sizeKB}KB, rowSize: $rowSize, dataWidth: ${dataType.getWidth}bits")
    
    numRows
  }
  
  // Convenience method to create SRAMConfig from size in KB
  def fromSizeKB[T <: Data](
    sizeKB: Double,
    rowSize: Int,
    dataType: T,
    tags: Seq[TensorType] 
  ): SRAMConfig[T] = {
    val numRows = calculateNumRows(sizeKB, rowSize, dataType)
    SRAMConfig(numRows, rowSize, dataType, tags)
  }
}

// IO Bundle for a single two-port SRAM
class TwoPortSRAMIO[T <: Data](config: SRAMConfig[T]) extends Bundle {
  val writeEnable = Input(Bool())
  val writeAddr = Input(UInt(log2Ceil(config.numRows).W))
  val writeData = Input(Vec(config.rowSize, config.dataType))
  val writeMask = Input(Vec(config.rowSize, Bool()))
  
  val readEnable = Input(Bool())
  val readValid = Output(Bool())
  val readAddr = Input(UInt(log2Ceil(config.numRows).W))
  val readData = Output(Vec(config.rowSize, config.dataType))
}

// Single two-port SRAM module
class TwoPortSRAM[T <: Data](config: SRAMConfig[T]) extends Module {
  val io = IO(new TwoPortSRAMIO(config))
  
  // Create the memory array
  val mem = SyncReadMem(config.numRows, Vec(config.rowSize, config.dataType)).suggestName("opera_sram")
  
  // Write port
  when(io.writeEnable) {
    mem.write(io.writeAddr, io.writeData, io.writeMask)
  }
  
  // Read port
  io.readData := mem.read(io.readAddr, io.readEnable)
  io.readValid := RegNext(io.readEnable)
}

// IO Bundle for the SRAM bank
class SRAMBankIO[T <: Data](configs: Seq[SRAMConfig[_ <: T]]) extends Bundle {
  val srams = MixedVec(
    configs.map { cfg =>
      new TwoPortSRAMIO(cfg)
    }
  )
}

// Bank of two-port SRAMs
class SRAMBank[T <: Data](configs: Seq[SRAMConfig[_ <: T]]) extends Module {
  require(configs.nonEmpty, "Must have at least one SRAM configuration")
  
  val io = IO(new SRAMBankIO(configs))
  
  // Instantiate each SRAM
  val srams = configs.zipWithIndex.map { case (config, idx) =>
    val sram = Module(new TwoPortSRAM(config))
    
    // Connect write port
    sram.io <> io.srams(idx)
    
    sram
  }
}