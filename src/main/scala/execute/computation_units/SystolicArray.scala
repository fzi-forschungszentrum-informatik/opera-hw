package openaccelerator.execute.computation_units

import chisel3._
import chisel3.util._

import openaccelerator.execute.{BaseComputationUnit, ComputationUnitConfig, CUBusPayload}  

// Processing Element for the systolic array
class ProcessingElement[T <: Data, U <: Data, V <: Data](
  aType: T, 
  bType: U, 
  dType: V
)(implicit ev1: T <:< SInt, ev2: U <:< SInt, ev3: V <:< SInt) extends Module {
  val io = IO(new Bundle {
    val weight_in = Input(bType)
    val weight_load_in = Input(Bool())
    val data_in = Input(aType)
    val data_valid_in = Input(Bool())
    val partial_sum_in = Input(dType)
    val partial_sum_valid_in = Input(Bool())
    
    val weight_out = Output(bType)
    val weight_load_out = Output(Bool())
    val data_out = Output(aType)
    val data_valid_out = Output(Bool())
    val partial_sum_out = Output(dType)
    val partial_sum_valid_out = Output(Bool())
  })
  
  // Stationary weight register
  val weight_reg = RegInit(0.S.asTypeOf(bType))
  
  // Pipeline registers for data propagation
  val data_reg = RegInit(0.S.asTypeOf(aType))
  val data_valid_reg = RegInit(false.B)

  val mac_result_reg = RegInit(0.S.asTypeOf(dType))
  val mac_result_valid_reg = RegInit(false.B)
  
  // Load weight when signal is high
  when(io.weight_load_in) {
    weight_reg := io.weight_in
  }

  io.weight_load_out := io.weight_load_in

  io.weight_out := weight_reg
  
  // Propagate data to next PE
  data_reg := io.data_in
  data_valid_reg := io.data_valid_in
  io.data_out := data_reg
  io.data_valid_out := data_valid_reg
  
  // Compute MAC operation: partial_sum_out = partial_sum_in + weight * data
  val product = weight_reg * io.data_in
  val mac_result = io.partial_sum_in + product

  mac_result_reg := mac_result.asTypeOf(dType)
  
  io.partial_sum_out := mac_result_reg
  io.partial_sum_valid_out := data_valid_reg
}

// Weight Stationary Systolic Array Computation Unit
class WeightStationarySystolicArray[T <: Data, U <: Data, V <: Data](
  config: ComputationUnitConfig[T, U, V]
)(implicit ev1: T <:< SInt, ev2: U <:< SInt, ev3: V <:< SInt) extends BaseComputationUnit(config) {
  
  // Create N x K array of processing elements
  val pe_array = Seq.fill(config.N, config.K) {
    Module(new ProcessingElement(config.ADataType, config.BDataType, config.DDataType))
  }

  val output_queue = Module(new Queue(io.out_c.bits.cloneType, 2))

  // Connect PE arrays between them
  for (row <- 0 until config.N) {
    for (col <- 0 until config.K) {
      val pe = pe_array(row)(col)

      // Initial weights flow vertically (bottom to top)
      if(row == config.N - 1){
        pe.io.weight_in := io.in_b.bits.data(col)
        pe.io.weight_load_in := io.in_b.bits.stationary && io.in_b.fire
      } 
      else{
        pe.io.weight_in := pe_array(row+1)(col).io.weight_out 
        pe.io.weight_load_in := pe_array(row+1)(col).io.weight_load_out
      } 

      // Data flows vertically (top to bottom)
      if (row == 0) {
        pe.io.data_in := ShiftRegister(io.in_a.bits.data(col), col)
        pe.io.data_valid_in := ShiftRegister(io.in_a.fire, col)
      } else {
        pe.io.data_in := pe_array(row - 1)(col).io.data_out
        pe.io.data_valid_in := pe_array(row - 1)(col).io.data_valid_out
      }
      
      // Partial sums flow horizontally (left to right)
      if (col == 0) {
        pe.io.partial_sum_in := ShiftRegister(io.in_d.bits.data(row), row)
        pe.io.partial_sum_valid_in := ShiftRegister(io.in_d.fire, row)
      } else {
        pe.io.partial_sum_in := pe_array(row)(col - 1).io.partial_sum_out
        pe.io.partial_sum_valid_in := pe_array(row)(col - 1).io.partial_sum_valid_out
      }

      if(col == config.K - 1)
      {
        output_queue.io.enq.valid := ShiftRegister(pe.io.partial_sum_valid_out, config.N-row)
        output_queue.io.enq.bits.data(row) := ShiftRegister(pe.io.partial_sum_out, config.N-row)
        output_queue.io.enq.bits.stationary := false.B
      } 
    }
  }

  io.out_c <> output_queue.io.deq
  
  // Default input ready signals
  io.in_a.ready := true.B
  io.in_b.ready := true.B
  io.in_d.ready := true.B

}