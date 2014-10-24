/*
 * Copyright (c) 2014 Pwootage
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
 * to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
 * FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package com.pwootage.sor1k.cpu

import com.pwootage.sor1k.IllegalInstructionException
import com.pwootage.sor1k.cpu.InstructionCodes._
import com.pwootage.sor1k.memory.MMU
import com.pwootage.sor1k.registers.Registers

/**
 * CPU core for OpenRisc 1000
 */
class OR1K(val reg: Registers, val mmu: MMU) {
  val instructions = new OR1KInterpretedInstructions(this)

  def executeStep(): Unit = {
    val instr = mmu.getInstruction(reg.pc)
    executeInstruction(instr)
  }

  def executeInstruction(i: Int): Unit = {
    val ins = new Instruction(i)
    opcodeLookupTable(ins.opcode)(ins)
  }

  private val opcodeLookupTable = new Array[Instruction => Any](1 << 6)
  for (i <- 0 until opcodeLookupTable.length) opcodeLookupTable(i) = { ins: Instruction => throw new IllegalInstructionException("Found unknown opcode: " + i)}

  opcodeLookupTable(L.Add._1) = { instr: Instruction =>
    //TODO: Scala is bad at constant inlining and I probably should manually inline them
    //ie there is one method call per case -.-
    instr.opcode4 match {
      case L.Add._3 => instructions.add(instr)
      case L.Addc._3 => instructions.addc(instr)
      case L.And._3 => instructions.and(instr)
      case L.Div._3 => instructions.div(instr)
      case L.Exths._3 => instr.opcode2 match {
        case L.Exths._2 => instructions.exths(instr)
        case L.Extbs._2 => instructions.extbs(instr)
        case L.Exthz._2 => instructions.exthz(instr)
        case L.Extbz._2 => instructions.extbz(instr)
      }
      case L.Divu._3 => instructions.divu(instr)
      case L.Cmov._3 => instructions.cmov(instr)
      case _ => throw new IllegalInstructionException("Invalid add opcode4: " + instr.opcode4)
    }
  }

  opcodeLookupTable(L.Addi._1) = { instr: Instruction => instructions.addi(instr)}
  opcodeLookupTable(L.Addic._1) = { instr: Instruction => instructions.addic(instr)}
  opcodeLookupTable(L.Andi._1) = { instr: Instruction => instructions.andi(instr)}
}
