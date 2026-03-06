/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera

import chisel3._
import chisel3.util.log2Ceil
import opera.scratchpad.{SRAMConfig}
import opera.execute.{ComputationUnitConfig} 
import opera.types.{TensorType, DataflowType} 
import opera.execute.BaseComputationUnit
import opera.execute.computation_units.{WeightStationarySystolicArray} 

case class BusInfo(
  totalRows: Int,
  busWidth: Int,
  elements: Int
) {
  val addrWidth = log2Ceil(totalRows)
  val elementBitWidth = busWidth / elements
  val elementByteWidth = elementBitWidth / 8
}

case class OPERAConfig[T <: Data, U <: Data, V <: Data](
    cuFactory: ComputationUnitConfig[T, U, V] => BaseComputationUnit[T, U, V],
    cu_config: ComputationUnitConfig[T, U, V],
    sram_configs: Seq[SRAMConfig[_ <: Data]]
){
    // TODO: In the future, support multiple computation units!

    require(
        sram_configs.filter(_.tags.contains(TensorType.Input)).forall(x => x.dataType.typeName == cu_config.ADataType.typeName && x.rowSize == cu_config.K),
        "All SRAMs with tag 'TensorType.Input' should have the same type as the Computation Unit ADataType and the rowSize should be equal to the Computation Unit K value"
    )

    require(
        sram_configs.filter(_.tags.contains(TensorType.Kernel)).forall(x => x.dataType.typeName == cu_config.BDataType.typeName && x.rowSize == cu_config.K),
        "All SRAMs with tag 'TensorType.Kernel' should have the same type as the Computation Unit BDataType and the rowSize should be equal to the Computation Unit K value"
    )

    require(
        sram_configs.filter(_.tags.contains(TensorType.Bias)).forall(x => x.dataType.typeName == cu_config.DDataType.typeName && x.rowSize == cu_config.N),
        "All SRAMs with tag 'TensorType.Bias' should have the same type as the Computation Unit DDataType and the rowSize should be equal to the Computation Unit N value"
    )

    require(
        sram_configs.filter(_.tags.contains(TensorType.Output)).forall(x => x.dataType.typeName == cu_config.DDataType.typeName && x.rowSize == cu_config.N),
        "All SRAMs with tag 'TensorType.Output' should have the same type as the Computation Unit DDataType and the rowSize should be equal to the Computation Unit N value"
    )

    private val inputSRAMs = sram_configs.filter(_.tags.contains(TensorType.Input))
    private val kernelSRAMs = sram_configs.filter(_.tags.contains(TensorType.Kernel))
    private val biasSRAMs = sram_configs.filter(_.tags.contains(TensorType.Bias))
    private val outputSRAMs = sram_configs.filter(_.tags.contains(TensorType.Output))

    val inputBus = BusInfo(
        totalRows = inputSRAMs.map(_.numRows).reduce(_ + _),
        busWidth = inputSRAMs(0).rowSizeBits,
        elements = inputSRAMs(0).rowSize
    )

    val kernelBus = BusInfo(
        totalRows = kernelSRAMs.map(_.numRows).reduce(_ + _),
        busWidth = kernelSRAMs(0).rowSizeBits,
        elements = kernelSRAMs(0).rowSize
    )

    val biasBus = BusInfo(
        totalRows = biasSRAMs.map(_.numRows).reduce(_ + _),
        busWidth = biasSRAMs(0).rowSizeBits,
        elements = biasSRAMs(0).rowSize
    )

    val outputBus = BusInfo(
        totalRows = outputSRAMs.map(_.numRows).reduce(_ + _),
        busWidth = outputSRAMs(0).rowSizeBits,
        elements = outputSRAMs(0).rowSize
    )

    val max_sram_rows = Seq(inputBus.totalRows, kernelBus.totalRows, biasBus.totalRows, outputBus.totalRows).max

    val srams_base_addresses = sram_configs.map(_.sizeElements).scanLeft(0)(_ + _).init

    val axi_addr_width = 32
    // TODO: Have different axi data widths for load and store?
    val axi_data_width = 64
    val max_rows_per_load_store = 64
} 

object OPERAConfigs {
    /*

    Default configuration (similar to Gemmini):

    16x16 systolic array, supports INT8 matrix multiplication with INT32 accumulation
    Two different SRAMs:
        1) Supports storing inputs and weights
        2) Supports storing outputs

    */
    val defaultConfig = OPERAConfig[SInt, SInt, SInt](
        // What kind of Computation Unit do we actually want to instantiate inside the Execute contorller?
        cuFactory = (config: ComputationUnitConfig[SInt, SInt, SInt]) => new WeightStationarySystolicArray[SInt, SInt, SInt](config),
        // Configure what the Computation Unit supports. If it supports different dataflows, then this is configured here.
        cu_config = ComputationUnitConfig(
            N = 16,
            K = 16,
            ADataType = SInt(8.W),
            BDataType = SInt(8.W),
            DDataType = SInt(32.W),
            supportedDataflows = Seq(DataflowType.WS)
        ),
        // Configure the SRAMs. We can define them both in terms of size, or in terms of elements. The tags define how the buses are connected.
        sram_configs = Seq(
            SRAMConfig(
                tags = Seq(TensorType.Input, TensorType.Kernel),
                numRows = 256, 
                rowSize = 16, 
                dataType = SInt(8.W)
            ),
            SRAMConfig.fromSizeKB(
                tags = Seq(TensorType.Bias, TensorType.Output),
                sizeKB = 8.0,
                rowSize = 16,
                dataType = SInt(32.W)
            )
        )
    ) 
} 
