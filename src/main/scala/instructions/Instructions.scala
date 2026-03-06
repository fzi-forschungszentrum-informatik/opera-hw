/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.instructions

import chisel3._
import chisel3.util._
import opera.types.{TensorTypeHW,DataflowTypeHW} 

abstract class Instruction extends Bundle 

class LoadInstruction(val total_rows: Int, val max_rows: Int) extends Instruction {
  val scratchpad_addr = UInt(log2Ceil(total_rows).W)
  val external_addr = UInt(32.W)
  val rows = UInt(log2Ceil(max_rows).W)
  val stride = UInt(8.W)
  val tensorType = TensorTypeHW()
}

class StoreInstruction(val total_rows: Int, val max_rows: Int) extends Instruction {
  val scratchpad_addr = UInt(log2Ceil(total_rows).W)
  val external_addr = UInt(32.W)
  val rows = UInt(log2Ceil(max_rows).W)
  val stride = UInt(8.W)
}

class ExecuteInstruction(
    input_rows: Int,
    kernel_rows: Int,
    bias_rows: Int,
    output_rows: Int,
    max_rows: Int
    ) extends Instruction {
  val input_scratchpad_addr = UInt(log2Ceil(input_rows).W)
  val kernel_scratchpad_addr = UInt(log2Ceil(kernel_rows).W)
  val bias_scratchpad_addr = UInt(log2Ceil(bias_rows).W)
  val output_scratchpad_addr = UInt(log2Ceil(output_rows).W)
  val preload_rows = UInt(log2Ceil(max_rows).W)
  val feeding_rows = UInt(log2Ceil(max_rows).W)
  val expected_output_rows = UInt(log2Ceil(max_rows).W)

  val dataflow = DataflowTypeHW()
  val preload = Bool()
}