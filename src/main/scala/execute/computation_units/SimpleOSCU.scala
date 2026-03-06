/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.execute.computation_units

import chisel3._
import chisel3.util._

import opera.execute.{BaseComputationUnit, ComputationUnitConfig, CUBusPayload} 

// Processing Element
class SimpleOSPE[T <: Data, U <: Data, V <: Data](
  aType: T, 
  bType: U, 
  dType: V
)(implicit ev1: T <:< SInt, ev2: U <:< SInt, ev3: V <:< SInt) extends Module {
  val io = IO(new Bundle {
    val weight_in = Input(bType)
    val data_in = Input(aType)
    val data_valid_in = Input(Bool())

    val partial_sum_out = Output(dType)
  })
  
  // Stationary output register
  val partial_sum_reg = RegInit(0.S.asTypeOf(dType))

  val mac_result = io.weight_in * io.data_in

  when(io.data_valid_in){
    partial_sum_reg := partial_sum_reg + mac_result.asTypeOf(dType)
  } 
  
  io.partial_sum_out := partial_sum_reg
}

// Simple OS Computation Unit
class SimpleOSCU[T <: Data, U <: Data, V <: Data](
  config: ComputationUnitConfig[T, U, V]
)(implicit ev1: T <:< SInt, ev2: U <:< SInt, ev3: V <:< SInt) extends BaseComputationUnit(config) {

  class TreeReduction[W <: Data](
    pes: Int,
    outputDataType: W
  )(implicit ev1: W <:< SInt) extends Module {
    val io = IO(new Bundle {
      val inputs = Input(Vec(pes, outputDataType))  // 8 inputs of 16 bits each
      val sum = Output(outputDataType)
    })
    
    // Tree reduction function
    def treeReduce(inputs: Seq[SInt], op: (SInt, SInt) => SInt): SInt = {
      inputs.length match {
        case 1 => inputs.head
        case n => 
          val (left, right) = inputs.splitAt(n / 2)
          op(treeReduce(left, op), treeReduce(right, op))
      }
    }
    
    // Apply tree reduction with addition
    val inputsAsSeq = io.inputs.toSeq.map(_.asInstanceOf[SInt])
    io.sum := treeReduce(inputsAsSeq, (a: SInt, b: SInt) => a + b)
  }
  
  // Create 1 x K array of processing elements
  val pe_array = Seq.fill(config.K) {
    Module(new SimpleOSPE(config.ADataType, config.BDataType, config.DDataType))
  }

  val partial_sum_reg = RegInit(0.S.asTypeOf(config.DDataType))

  when(io.in_d.fire){
    partial_sum_reg := io.in_d.bits.data(0)
  } 

  for (col <- 0 until config.K) {
    val pe = pe_array(col)
    pe.io.weight_in := io.in_b.bits.data(col)
    pe.io.data_in := io.in_a.bits.data(col)
    pe.io.data_valid_in := io.in_a.fire && io.in_b.fire
  } 

  val peOutputs = VecInit(pe_array.map(_.io.partial_sum_out))
  val treeReduction = Module(new TreeReduction(config.K, config.DDataType))

  treeReduction.io.inputs := peOutputs

  val no_input_anymore = !io.in_a.fire && !io.in_b.fire
  when(no_input_anymore){
    io.out_c.valid := true.B
  } 
  .otherwise{
    io.out_c.valid := false.B
  } 

  io.out_c.bits.data(0) := treeReduction.io.sum + partial_sum_reg
} 