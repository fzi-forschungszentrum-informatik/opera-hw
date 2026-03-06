/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.chipyard

import chisel3._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem.{BaseSubsystem, PBUS, CBUS, SBUS, CacheBlockBytes}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.regmapper.{RegisterRouterParams,RegisterRouter, RegField, RegFieldDesc, RegFieldGroup} 
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.ResourcePermissions
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.HasTLControlRegMap
import opera.{OPERA, OPERAConfig, OPERAConfigs}
import opera.types.{DataflowType, TensorType, ChiselTypeToString} 
import opera.chipyard.{DecoupledRegMapperFields, RegmapArtifact}

case class OPERAParams(
  address: BigInt = 0x4000,
  externallyClocked: Boolean = false
)

case object OPERAKey extends Field[OPERAParams]

abstract class OPERAMMIO[T <: Data, U <: Data, V <: Data](beatBytes: Int, operaConfig: OPERAConfig[T,U,V] = OPERAConfigs.defaultConfig)(implicit p: Parameters) 
  extends RegisterRouter(
    RegisterRouterParams(
      name = "opera",
      compat = Seq("fzi,opera"),
      base = 0x70000000L,
      beatBytes = beatBytes
    )
  )
  with HasInterruptSources
  with HasClockDomainCrossing {

  def nInterrupts = 4

  private def ofType(x: DataflowType) = Seq(ResourceString(x.toString))
  private def ofType(x: TensorType) = Seq(ResourceString(x.toString))
  private def ofDataType(x: Data) = Seq(ResourceString(ChiselTypeToString.toTypeString(x)))
  private def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

  val sram_configs_for_dts = operaConfig.sram_configs.zipWithIndex.map{ case (config, idx) => {
    s"sram@$idx" -> Seq(ResourceMap( value = Map(
      "address" -> (ofInt(0) ++ ofInt(config.sizeBytes)),
      "datatype" -> ofDataType(config.dataType),
      "tags" -> config.tags.flatMap(ofType),
      "elements_per_row" -> ofInt(config.rowSize),
      "rows" -> ofInt(config.numRows)
    )))
  }}.toMap 

  val cu_configs_for_dts = Map(
    "dataflows" -> operaConfig.cu_config.supportedDataflows.flatMap(ofType),
    "N" -> ofInt(operaConfig.cu_config.N),
    "K" -> ofInt(operaConfig.cu_config.K),
    "input_datatype" -> ofDataType(operaConfig.cu_config.ADataType),
    "kernel_datatype" -> ofDataType(operaConfig.cu_config.BDataType),
    "bias_datatype" -> ofDataType(operaConfig.cu_config.DDataType)
  )

  override def extraResources(resources: ResourceBindings) = cu_configs_for_dts ++ sram_configs_for_dts

  private val paramsOpt: Option[OPERAParams] = p.lift(OPERAKey)
  require(paramsOpt.nonEmpty, "No OPERAKey in Parameters")
  private def params = paramsOpt.get

  val mem_node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name    = "mem",
      id      = IdRange(0, 1 << 1),
      aligned = true))))) 

  override lazy val module = new LazyModuleImp(this) {
    val impl = Module(new OPERA(OPERAConfigs.defaultConfig))

    val (mem_axi, _) = mem_node.out(0)

    /* Store AXI interface mapping */
    // TODO: manage id, lock and qos correctly for more complex examples
    mem_axi.aw.bits.id := 0.U
    mem_axi.aw.bits.addr := impl.io.store_axi.aw.bits.addr
    mem_axi.aw.bits.len := impl.io.store_axi.aw.bits.len
    mem_axi.aw.bits.size := impl.io.store_axi.aw.bits.size
    mem_axi.aw.bits.burst := impl.io.store_axi.aw.bits.burst
    mem_axi.aw.bits.lock := 0.U
    mem_axi.aw.bits.cache := AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_BUFFERABLE
    mem_axi.aw.bits.prot := AXI4Parameters.PROT_PRIVILEGED
    mem_axi.aw.bits.qos := 0.U
    mem_axi.aw.valid := impl.io.store_axi.aw.valid
    impl.io.store_axi.aw.ready := mem_axi.aw.ready 

    mem_axi.w.bits.data := impl.io.store_axi.w.bits.data
    mem_axi.w.bits.strb := impl.io.store_axi.w.bits.strb
    mem_axi.w.bits.last := impl.io.store_axi.w.bits.last
    mem_axi.w.valid := impl.io.store_axi.w.valid
    impl.io.store_axi.w.ready := mem_axi.w.ready

    impl.io.store_axi.b.valid := mem_axi.b.valid
    mem_axi.b.ready := impl.io.store_axi.b.ready
    impl.io.store_axi.b.bits.ok := true.B

    /* Load AXI interface mapping */
    // TODO: manage id, lock and qos correctly for more complex examples
    mem_axi.ar.bits.id := 0.U
    mem_axi.ar.bits.addr := impl.io.load_axi.ar.bits.addr
    mem_axi.ar.bits.len := impl.io.load_axi.ar.bits.len
    mem_axi.ar.bits.size := impl.io.load_axi.ar.bits.size
    mem_axi.ar.bits.burst := impl.io.load_axi.ar.bits.burst
    mem_axi.ar.bits.lock := 0.U
    mem_axi.ar.bits.cache := AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_BUFFERABLE
    mem_axi.ar.bits.prot := AXI4Parameters.PROT_PRIVILEGED
    mem_axi.ar.bits.qos := 0.U
    mem_axi.ar.valid := impl.io.load_axi.ar.valid
    impl.io.load_axi.ar.ready := mem_axi.ar.ready

    impl.io.load_axi.r.bits.data := mem_axi.r.bits.data
    impl.io.load_axi.r.bits.last := mem_axi.r.bits.last
    impl.io.load_axi.r.valid := mem_axi.r.valid
    mem_axi.r.ready := impl.io.load_axi.r.ready

    // MMIO Registers
    val reg_load_empty_int_enable = RegInit(Bool(), false.B)
    val reg_store_empty_int_enable = RegInit(Bool(), false.B)
    val reg_execute_empty_int_enable = RegInit(Bool(), false.B)
    val reg_all_empty_int_enable = RegInit(Bool(), false.B)

    interrupts(0) := impl.io.load_empty && reg_load_empty_int_enable
    interrupts(1) := impl.io.store_empty && reg_store_empty_int_enable
    interrupts(2) := impl.io.execute_empty && reg_execute_empty_int_enable
    interrupts(3) := impl.io.all_empty && reg_all_empty_int_enable

    val mapping = Seq(
      0x0 -> RegFieldGroup("interrupt_enable", Some("Interrupt Enable Registers"), Seq(
          RegField(1, reg_load_empty_int_enable, RegFieldDesc("load_empty_ie", "Load queue empty interrupt enable", reset = Some(0))),
          RegField(1, reg_store_empty_int_enable, RegFieldDesc("store_empty_ie", "Store queue empty interrupt enable", reset = Some(0))),
          RegField(1, reg_execute_empty_int_enable, RegFieldDesc("execute_empty_ie", "Execute queue empty interrupt enable", reset = Some(0))),
          RegField(1, reg_all_empty_int_enable, RegFieldDesc("all_empty_ie", "All queues empty interrupt enable", reset = Some(0))),
        )
      ),
      // In case the user wants to poll the status of the accelerator...
      0x4 -> RegFieldGroup("interrupt_flags", Some("Interrupt Flags Registers"), Seq(
          RegField.r(1, impl.io.load_empty, RegFieldDesc("load_empty_if", "Load queue empty interrupt flag")), 
          RegField.r(1, impl.io.store_empty, RegFieldDesc("store_empty_if", "Store queue empty interrupt flag")), 
          RegField.r(1, impl.io.execute_empty, RegFieldDesc("execute_empty_if", "Execute queue empty interrupt flag")), 
          RegField.r(1, impl.io.all_empty, RegFieldDesc("all_empty_if", "All queues empty interrupt flag")),        
        )
      ),
      0x100 -> RegFieldGroup("load_instruction", Some("Load Instruction Queue Input"), DecoupledRegMapperFields(impl.io.load_in)),
      0x200 -> RegFieldGroup("store_instruction", Some("Store Instruction Queue Input"), DecoupledRegMapperFields(impl.io.store_in)),
      0x300 -> RegFieldGroup("execute_instruction", Some("Execute Instruction Queue Input"), DecoupledRegMapperFields(impl.io.execute_in)),
    )

    regmap(mapping: _*)
    RegmapArtifact.emit("opera", address, mapping)
  }
}

/** Specialize the generic OPERAMMIO to make it attachable to a TileLink interconnect. */
class OPERATL(beatBytes: Int)(implicit p: Parameters)
  extends OPERAMMIO(beatBytes) with HasTLControlRegMap

/** Specialize the generic OPERAMMIO to make it attachable to an AXI4 interconnect. */
class OPERAAXI(beatBytes: Int)(implicit p: Parameters)
  extends OPERAMMIO(beatBytes) with HasAXI4ControlRegMap

trait CanHavePeripheryOPERA {
  this: BaseSubsystem =>

  if(p.lift(OPERAKey).isDefined){ 

    val cbus = locateTLBusWrapper(CBUS)
    val sbus = locateTLBusWrapper(SBUS)

    val operaMod = LazyModule(new OPERATL(cbus.beatBytes))

    cbus.coupleTo("opera_regs") {
      operaMod.controlXing() := TLFragmenter(cbus.beatBytes, cbus.blockBytes) := _
    }

    sbus.coupleFrom(s"opera_mem") {
        _ :=* (TLBuffer()
          := TLFIFOFixer(TLFIFOFixer.all)
          := AXI4ToTL() // convert to TL
          := AXI4UserYanker(capMaxFlight=Some(1)) // remove user field on AXI interface. need but in reality user intf. not needed
          := AXI4Fragmenter() // deal with multi-beat xacts
          := AXI4IdIndexer(1)
          := AXI4Buffer()
          := operaMod.mem_node)
      }
    ibus.fromSync := operaMod.intXing(NoCrossing)
    InModuleBody { operaMod.module.clock := cbus.module.clock }
    InModuleBody { operaMod.module.reset := cbus.module.reset }

    operaMod
  } 
}

class WithOPERA(base_address: BigInt = BigInt(0x70000000L), externallyClocked : Boolean = false) extends Config((site, here, up) => {
  case OPERAKey => {
    Some(OPERAParams(address = base_address, externallyClocked = externallyClocked))
  }
})