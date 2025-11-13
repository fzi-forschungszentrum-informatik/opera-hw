package openaccelerator.execute

import chisel3._
import chisel3.util._

// Bundle for the data payload (stationary + data vector)
class CUBusPayload[T <: Data](numElements: Int, dataType: T) extends Bundle {
  val stationary = Bool()
  val data = Vec(numElements, dataType.cloneType)
}

// Configuration for the entire ComputationUnit
case class ComputationUnitConfig[T <: Data, U <: Data, V <: Data](
  N: Int,
  K: Int,
  ADataType: T,
  BDataType: U,
  DDataType: V,
)

// Abstract base class for ComputationUnit
abstract class BaseComputationUnit[T <: Data, U <: Data, V <: Data](config: ComputationUnitConfig[T, U, V]) extends Module {
  val io = IO(new Bundle {
    // Input buses - Flipped Decoupled for inputs
    val in_a = Flipped(Decoupled(new CUBusPayload(config.K, config.ADataType)))
    val in_b = Flipped(Decoupled(new CUBusPayload(config.K, config.BDataType)))
    val in_d = Flipped(Decoupled(new CUBusPayload(config.N, config.DDataType)))
    
    // Output bus - Decoupled for output
    val out_c = Decoupled(new CUBusPayload(config.N, config.DDataType))
  })
}