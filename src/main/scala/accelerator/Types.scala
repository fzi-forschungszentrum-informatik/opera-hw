/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.types

import chisel3.ChiselEnum
import chisel3._

object ChiselTypeToString {

  def toTypeString(data: Data): String = data match {

    // ---------- Integers ----------
    case s: SInt =>
      val w = s.getWidth
      require(w > 0, "SInt width must be known")
      s"int$w"

    case u: UInt =>
      val w = u.getWidth
      require(w > 0, "UInt width must be known")
      s"uint$w"

    // ---------- Unsupported ----------
    case other =>
      throw new IllegalArgumentException(
        s"Unsupported Chisel type: ${other.getClass.getSimpleName}"
      )
  }
}

// These ones are used during elaboration time to configure the accelerator
sealed trait TensorType
object TensorType {
  case object Input extends TensorType
  case object Kernel extends TensorType
  case object Bias extends TensorType
  case object Output extends TensorType

  def toHW(m: TensorType): TensorTypeHW.Type = m match {
    case Input => TensorTypeHW.Input
    case Kernel => TensorTypeHW.Kernel
    case Bias => TensorTypeHW.Bias
    case Output => TensorTypeHW.Output
  }
}

sealed trait DataflowType
object DataflowType {
  case object IS extends DataflowType
  case object WS extends DataflowType
  case object OS extends DataflowType

  def toHW(m: DataflowType): DataflowTypeHW.Type = m match {
    case IS => DataflowTypeHW.IS
    case WS => DataflowTypeHW.WS
    case OS => DataflowTypeHW.OS
  }
}

// These ones are a Chisel HW construct, can be used to generate HW
object TensorTypeHW extends ChiselEnum {
  val Input, Kernel, Bias, Output = Value
}

object DataflowTypeHW extends ChiselEnum {
  val IS, WS, OS = Value
}