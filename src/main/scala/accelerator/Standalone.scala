/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera

import chisel3._
import circt.stage.ChiselStage
import opera.{OPERA, OPERAConfigs}  

object SystemVerilog extends App {
  // Configuration options for the CIRCT FIRRTL-to-SystemVerilog conversion
  val firtoolOptions = Array(
    "--lowering-options=" + List(
      // This are taken from the common.mk file in the original Chipyard repository
      "noAlwaysComb", "verifLabels", "emittedLineLength=2048", "disallowPortDeclSharing",
      // Disable local variables to ensure Yosys compatibility
      // @see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
      "disallowLocalVariables",
      // Disable packed arrays to ensure Yosys compatibility
      "disallowPackedArrays",
      // Use [<filename>:<line>] format for source location annotations
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _) // Join options with commas
  )

  // Options for mem configs
  //firtoolOptions :+ " --repl-seq-mem"
  //firtoolOptions :+ " --repl-seq-mem-file=OPERA.seqmem.conf"

  ChiselStage.emitSystemVerilogFile(new OPERA(OPERAConfigs.defaultConfig), args, firtoolOptions)
}