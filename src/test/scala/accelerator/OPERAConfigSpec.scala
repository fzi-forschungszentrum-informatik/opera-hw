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
import opera.OPERAConfigs

class OPERAConfigSpec extends AnyFlatSpec with Matchers {
  
  behavior of "OPERAConfigs.defaultConfig"
  
  it should "create a valid default configuration" in {

    try {
        OPERAConfigs.defaultConfig
        succeed
    } catch {
        case e: IllegalArgumentException =>
        println(s"Require failed: ${e.getMessage}")
        fail(s"Configuration validation failed: ${e.getMessage}")
        case e: Throwable =>
        println(s"Unexpected error: ${e.getMessage}")
        e.printStackTrace()
        fail(s"Unexpected exception: ${e.getMessage}")
    }
  }
} 