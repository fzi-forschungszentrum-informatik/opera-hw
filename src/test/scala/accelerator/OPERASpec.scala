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
import opera.{OPERA, OPERAConfigs} 

class OPERASpec extends AnyFlatSpec with ChiselSim with Matchers {
  
  behavior of "OPERA"
  
  it should "check default configuration" in {
    simulate(new OPERA(OPERAConfigs.defaultConfig)) { dut => 
        succeed
    }  
  }
} 