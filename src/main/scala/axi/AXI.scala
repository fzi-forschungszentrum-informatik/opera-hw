/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.axi

import chisel3._
import chisel3.util._

class SimpleAXI4ReadAddrPayload(addrWidth: Int, idWidth: Int = 1) extends Bundle {
  val addr  = UInt(addrWidth.W)
  val len   = UInt(8.W)     // beats - 1
  val size  = UInt(3.W)
  val burst = UInt(2.W)     // INCR = 1
}

class SimpleAXI4ReadDataPayload(dataWidth: Int, idWidth: Int = 1) extends Bundle {
  val data  = UInt(dataWidth.W)
  val last  = Bool()
}

class SimpleAXI4ReadIO(addrWidth: Int = 64, dataWidth: Int = 64) extends Bundle {
  // Master drives AR, so slave sees Flipped(Decoupled)
  val ar = Decoupled(new SimpleAXI4ReadAddrPayload(addrWidth))
  // Slave drives R, so master sees Flipped(Decoupled)
  val r  = Flipped(Decoupled(new SimpleAXI4ReadDataPayload(dataWidth)))
}

class SimpleAXI4WriteAddrPayload(addrWidth: Int) extends Bundle {
  val addr  = UInt(addrWidth.W)
  val len   = UInt(8.W)
  val size  = UInt(3.W)
  val burst = UInt(2.W)
}

class SimpleAXI4WriteDataPayload(dataWidth: Int) extends Bundle {
  val data  = UInt(dataWidth.W)
  val strb  = UInt((dataWidth / 8).W)
  val last  = Bool()
}

class SimpleAXI4WriteRespPayload extends Bundle {
  val ok = Bool()   // BRESP simplified
}

class SimpleAXI4WriteIO(addrWidth: Int = 32, dataWidth: Int = 64) extends Bundle {
  val aw = Decoupled(new SimpleAXI4WriteAddrPayload(addrWidth))
  val w  = Decoupled(new SimpleAXI4WriteDataPayload(dataWidth))
  val b  = Flipped(Decoupled(new SimpleAXI4WriteRespPayload))
}