/*
 * Copyright (c) 2026 FZI Forschungszentrum Informatik
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package opera.chipyard

import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc} 
import opera.instructions.{Instruction}
import opera.types.{TensorTypeHW, DataflowTypeHW} 
import chisel3.reflect.DataMirror
import freechips.rocketchip.util.ElaborationArtefacts
import freechips.rocketchip.diplomacy.{AddressSet}

object DecoupledRegMapperFields {

  def roundUpToNearest(n: Int): Int = {
    val limits = List(8, 16, 32)
    limits.find(_ >= n).getOrElse(limits.last)
  }

  def apply[T <: Instruction](
      io:   DecoupledIO[T], alignment: Int = 4
  ): Seq[RegField] = {

    val alignment_bits = alignment * 8

    val fields = io.bits.elements.toSeq.reverse      // field order preserved
    val regs   = fields.map { case (fname, elem) =>     // one backing reg per field
      RegInit(0.U(DataMirror.widthOf(elem).get.W)).suggestName("reg_" + fname)
    }.toArray

    val reg_valid = RegInit(false.B)

    // ================= Bundle -> HW Connection ======================
    fields.zip(regs).foreach { case ((fname, elem), r) =>
      val element_width = DataMirror.widthOf(elem).get
      io.bits.elements(fname) := r.asTypeOf(elem.cloneType)
    }
    io.valid := reg_valid

    // Auto-fire behavior
    when(io.valid && io.ready) { reg_valid := false.B }

    // ================= Create RegFields =============================
    val fieldRegs = collection.mutable.ListBuffer[RegField]()

    for (((fname, elem), r) <- fields.zip(regs)) {
      val w = roundUpToNearest(r.getWidth)
      fieldRegs += RegField(w, r, RegFieldDesc(fname,fname))
      val padding = alignment_bits - w 
      if(padding != 0) fieldRegs += RegField(padding, RegFieldDesc.reserved)
    }

    // Append valid + ready status
    fieldRegs += RegField(8, reg_valid, RegFieldDesc("valid","valid"))
    fieldRegs += RegField.r(8, io.ready, RegFieldDesc("ready","ready"))

    // Wrap everything into a single RegFieldGroup
    fieldRegs.toSeq
  }
}

object RegmapArtifact {

  def emitChiselEnum(e: ChiselEnum, sb: StringBuilder): Unit = {

    val enumName = e.getClass.getSimpleName.stripSuffix("$").toUpperCase

    sb ++= "typedef enum {\n"
    e.allWithNames.foreach { case (enumVal, name) =>
      val value = enumVal.litValue.toInt
      sb ++=s"\t${enumName}_$name = $value,\n"
    }
    sb ++= s"} ${enumName};\n"
  } 

  def emit(name: String, base_address: Seq[AddressSet], regmap: Seq[(Int, Seq[RegField])], alignment: Int = 4): Unit = {

    val actual_base_address = base_address(0).base
    val upper = name.toUpperCase
    val sb = new StringBuilder

    sb ++= s"#ifndef ${upper}_H\n#define ${upper}_H\n\n"
    sb ++= "#include <stdint.h>\n#include <stdbool.h>\n#include <stddef.h>\n\n"

    // ==========================================================================
    // 1) Build structured register + bitfield map
    // ==========================================================================

    //val regs  =
    val regDescs = regmap.flatMap { case (byte, seq) =>
      seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bit, f) =>
        val anonName = s"unnamedRegField${byte.toHexString}_${bit}"
        (f.desc.map{ _.group}.get.get) -> (f, byte, bit)
      }
    }

    emitChiselEnum(TensorTypeHW, sb)
    emitChiselEnum(DataflowTypeHW, sb)

    val interrupts_registers = Set("interrupt_enable","interrupt_flags")
    val handshake_signals = Set("valid","ready", "reserved") // reserved is not a handshake signal, but we don't want to print it into the prototype of the write functions in the C header

    // Typedef for instructions
    regDescs.filterNot { case (group, _) => interrupts_registers.contains(group)}.groupBy(_._1).flatMap {case (group, items) => 
        sb ++= s"typedef struct __attribute__((packed, aligned($alignment))) {\n"
        var fields = collection.mutable.ListBuffer[(String, String)]()
        var reserved_idx = 0
        items.map { case (_, (regfield, byte, bit)) =>
          val name = regfield.desc.map{ _.name}.get
          fields.append((f"uint${regfield.width}_t", name))
          if(name == "reserved") {
            sb ++= f"\tvolatile uint8_t reserved_${reserved_idx}[${regfield.width / 8}];\n"
            reserved_idx = reserved_idx + 1
          }
          else sb ++= f"\tvolatile uint${regfield.width}_t ${name};\n"
        }
        sb ++= s"} ${group}_regs_t;\n\n"

        // Add sanity checks
        var offset = 0
        items.map { case (_, (regfield, byte, bit)) =>
          val name = regfield.desc.map{ _.name}.get
          val byteOffset = offset / 8
          if(name != "reserved") sb ++= f"_Static_assert(offsetof(${group}_regs_t, ${name}) == 0x$byteOffset%X, \"Bad $name offset\");\n"
          offset = offset + regfield.width
        }
        sb ++= "\n"

        val has_handshake_signals = fields.filter{ case (_, field) => handshake_signals.contains(field)}.length > 0
        fields = fields.filterNot{ case (_, field) => handshake_signals.contains(field)}

        val register_base_address = actual_base_address + items(0)._2._2
        // Create pointer to struct to write to registers easier
        sb ++= f"volatile ${group}_regs_t * $group%s_regs = (${group}_regs_t *) 0x$register_base_address%X;\n"

        // Create inline function to write even easier
        sb ++= f"#if ${if(has_handshake_signals) 1 else 0}\n"
        sb ++= f"inline void write_${group}("
        fields.init.foreach { case (dtype, fieldName) =>
          sb ++= f"${dtype} ${fieldName},"
        }
        sb ++= f"${fields.last._1} ${fields.last._2}){\n"
        // Wait for ready
        sb ++= f"\twhile(${group}_regs->ready != 1){};\n"
        // Write all fields
        fields.foreach { case (dtype, fieldName) =>
          sb ++= f"\t${group}_regs->${fieldName} = ${fieldName};\n"
        }
        // Make valid 1
        sb ++= f"\t${group}_regs->valid = 1;\n"
        sb ++= "}\n"
        sb ++= "#endif\n\n"
    }

    // Defines for interrupts
    regDescs.filter { case (group, _) => interrupts_registers.contains(group)}.groupBy(_._1).flatMap {case (group, items) => 
        val register_base_address = actual_base_address + items(0)._2._2
        sb ++= f"volatile uint32_t * $group%s_regs = (uint32_t *) 0x$register_base_address%X;\n"
        items.map { case (_, (regfield, byte, bit)) =>
            val fieldName = regfield.desc.map{ _.name}.get
            sb ++= s"#define ${fieldName.toUpperCase} ((uint32_t)1 << ${bit})\n"
        }
        var is_enable = false
        if(group == "interrupt_enable") is_enable = true
        items.map { case (_, (regfield, byte, bit)) =>
            val fieldName = regfield.desc.map{ _.name}.get
            if(is_enable){
              sb ++= s"inline void set_${fieldName}(){\n"
              sb ++= f"\tuint32_t val = *$group%s_regs;\n"
              sb ++= f"\tval |= $fieldName%S;\n"
              sb ++= f"\t*$group%s_regs = val;\n"
              sb ++= s"}\n"
              sb ++= s"inline void clear_${fieldName}(){\n"
              sb ++= f"\tuint32_t val = *$group%s_regs;\n"
              sb ++= f"\tval &= ~$fieldName%S;\n"
              sb ++= f"\t*$group%s_regs = val;\n"
              sb ++= s"}\n\n"
            } 
            else{
              sb ++= s"inline uint8_t is_${fieldName}(){\n"
              sb ++= f"\tuint32_t val = *$group%s_regs;\n"
              sb ++= f"\treturn (uint8_t)((val >> $bit) & (uint32_t)1);\n"
              sb ++= s"}\n"
              sb ++= s"inline void clear_${fieldName}(){\n"
              sb ++= f"\tuint32_t val = *$group%s_regs;\n"
              sb ++= f"\tval &= $fieldName%S;\n"
              sb ++= f"\t*$group%s_regs = val;\n"
              sb ++= s"}\n\n"
            } 
        }

    }

    sb ++= "\n#endif\n"
    ElaborationArtefacts.add(s"${name}.h", sb.toString)
  }
}