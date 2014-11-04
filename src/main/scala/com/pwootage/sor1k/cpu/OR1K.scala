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

import com.pwootage.sor1k.{CPUException, IllegalInstructionException}
import com.pwootage.sor1k.cpu.InstructionCodes._
import com.pwootage.sor1k.memory.MMU
import com.pwootage.sor1k.registers.Registers

/**
 * CPU core for OpenRisc 1000
 */
class OR1K(val reg: Registers, val mmu: MMU) {
  //  val instructions = new OR1KInterpretedInstructions(this)
  val instructions = new FastOR1KInterpretedInstructions(this)

  /** Indicates whether the CPU should execute a delay slot before going to NPC */
  var delaySlot = false

  def except(vector: Int, pcOff: Int = 0): Unit = {
    if (delaySlot) {
      reg.epcr.set(reg.pc - 4 + pcOff)
    } else {
      reg.epcr.set(reg.pc + pcOff)
    }
    reg.esrr.set(reg.sr.get)
    reg.sr.dme = 0
    reg.sr.ime = 0
    reg.sr.sm = 1
    reg.sr.iee = 0
    reg.sr.tee = 0
    reg.npc = vector
    delaySlot = false
  }

  def executeStep(): Unit = {
    val instr = mmu.getInstruction(reg.pc)
    executeInstruction(instr)
    if (delaySlot) {
      reg.pc += 4
      delaySlot = false
    } else {
      reg.pc = reg.npc
      reg.npc = reg.pc + 4
    }
  }

  def dumpRegistersAndInstruction(): Unit = {
    val instr = try mmu.getInstruction(reg.pc) catch {
      case e: Throwable => -1
    }
    val parsed = Instruction(instr)
    val fmt = "%08x"
    println(s"CPU state: PC = 0x${reg.pc.formatted(fmt)}, NPC = 0x${reg.npc.formatted(fmt)}, [PC] = 0x${instr.formatted(fmt)}")
    println(s"Insruction: opcode = 0x${parsed.opcode.formatted("%02x")}, regD = ${parsed.regD}, regA = ${parsed.regA}, regB = ${parsed.regB}")
    println("Registers:")
    for (i <- 0 until 8) {
      print(s"${(i * 4).formatted("%02d")} ")
      for (j <- 0 until 4) {
        print(s"${reg.gp(i * 4 + j).formatted(fmt)} ")
      }
      println()
    }
  }

  def executeInstruction(i: Int): Unit = {
    val ins = new Instruction(i)
    opcodeLookupTable(ins.opcode)(ins)
  }

  private val opcodeLookupTable = new Array[Instruction => Any](1 << 6)
  for (i <- 0 until opcodeLookupTable.length) opcodeLookupTable(i) = { ins: Instruction => throw new IllegalInstructionException("Found unknown opcode: " + i)}

  opcodeLookupTable(L.J) = instructions.j _
  opcodeLookupTable(L.Jal) = instructions.jal _
  opcodeLookupTable(L.Bnf) = instructions.bnf _
  opcodeLookupTable(L.Bf) = instructions.bf _
  opcodeLookupTable(L.Movhi) = instructions.movhi _
  opcodeLookupTable(L.Sys) = { instr: Instruction =>
    instr.opcode16 match {
      case L.Sys2 => instructions.sys(instr)
      case L.Trp2 => instructions.trp(instr)
      case _ => throw new IllegalInstructionException("Invalid opcode16: " + instr.opcode16)
    }
  }
  opcodeLookupTable(L.Rfe) = instructions.rfe _
  opcodeLookupTable(L.Jr) = instructions.jr _
  opcodeLookupTable(L.Jalr) = instructions.jalr _
  opcodeLookupTable(L.Nop) = instructions.nop _
  opcodeLookupTable(L.Lwa) = instructions.lwa _
  opcodeLookupTable(L.Lwz) = instructions.lwz _
  opcodeLookupTable(L.Lws) = instructions.lws _
  opcodeLookupTable(L.Lbz) = instructions.lbz _
  opcodeLookupTable(L.Lbs) = instructions.lbs _
  opcodeLookupTable(L.Lhz) = instructions.lhz _
  opcodeLookupTable(L.Lhs) = instructions.lhs _
  opcodeLookupTable(L.Addi) = instructions.addi _
  opcodeLookupTable(L.Addic) = instructions.addic _
  opcodeLookupTable(L.Andi) = instructions.andi _
  opcodeLookupTable(L.Ori) = instructions.ori _
  opcodeLookupTable(L.Xori) = instructions.xori _
  opcodeLookupTable(L.Muli) = instructions.muli _
  opcodeLookupTable(L.Mfspr) = instructions.mfspr _
  opcodeLookupTable(L.Rori) = { instr: Instruction =>
    instr.opcode2_bit6 match {
      case L.Slli2 => instructions.slli(instr)
      case L.Srai2 => instructions.srai(instr)
      case L.Srli2 => instructions.srli(instr)
      case L.Rori2 => instructions.rori(instr)
      case _ => throw new IllegalInstructionException("Invalid add opcode2_bitb: " + instr.opcode2_bit6)
    }
  }
  opcodeLookupTable(L.Sfeqi) = { instr: Instruction =>
    instr.regD match {
      case L.Sfeqi2 => instructions.sfeqi(instr)
      case L.Sfnei2 => instructions.sfnei(instr)
      case L.Sfgtui2 => instructions.sfgtui(instr)
      case L.Sfgeui2 => instructions.sfgeui(instr)
      case L.Sfltui2 => instructions.sfltui(instr)
      case L.Sfleui2 => instructions.sfleui(instr)
      case L.Sfgtsi2 => instructions.sfgtsi(instr)
      case L.Sfgesi2 => instructions.sfgesi(instr)
      case L.Sfltsi2 => instructions.sfltsi(instr)
      case L.Sflesi2 => instructions.sflesi(instr)
      case _ => throw new IllegalInstructionException("Invalid set flag immediate opcode: " + instr.regD)
    }
  }
  opcodeLookupTable(L.Mtspr) = instructions.mtspr _
  opcodeLookupTable(L.Swa) = instructions.swa _
  opcodeLookupTable(L.Sw) = instructions.sw _
  opcodeLookupTable(L.Sb) = instructions.sb _
  opcodeLookupTable(L.Sh) = instructions.sh _
  opcodeLookupTable(L.Add) = { instr: Instruction =>
    //TODO: Scala is bad at constant inlining and I probably should manually inline them
    //ie there is one method call per case -.-
    instr.opcode4 match {
      case L.Add3 => instructions.add(instr)
      case L.Addc3 => instructions.addc(instr)
      case L.Sub3 => instructions.sub(instr)
      case L.And3 => instructions.and(instr)
      case L.Or3 => instructions.or(instr)
      case L.Xor3 => instructions.xor(instr)
      case L.Mul3 => instructions.mul(instr)
      case L.Sll3 => instr.opcode2 match {
        case L.Sll2 => instructions.sll(instr)
        case L.Sra2 => instructions.sra(instr)
        case L.Srl2 => instructions.srl(instr)
        case L.Ror2 => instructions.ror(instr)
      }
      case L.Div3 => instructions.div(instr)
      case L.Mulu3 => instructions.mulu(instr)
      case L.Exths3 => instr.opcode2 match {
        case L.Exths2 => instructions.exths(instr)
        case L.Extbs2 => instructions.extbs(instr)
        case L.Exthz2 => instructions.exthz(instr)
        case L.Extbz2 => instructions.extbz(instr)
      }
      case L.Divu3 => instructions.divu(instr)
      case L.Cmov3 => instructions.cmov(instr)
      case _ => throw new IllegalInstructionException("Invalid add opcode4: " + instr.opcode4)
    }
  }

  opcodeLookupTable(L.Sfeq) = { instr: Instruction =>
    //This is actually an opcode but hey, it's the right place
    instr.regD match {
      case L.Sfeq2 => instructions.sfeq(instr)
      case L.Sfne2 => instructions.sfne(instr)
      case L.Sfgtu2 => instructions.sfgtu(instr)
      case L.Sfgeu2 => instructions.sfgeu(instr)
      case L.Sfltu2 => instructions.sfltu(instr)
      case L.Sfl3u2 => instructions.sfleu(instr)
      case L.Sfgts2 => instructions.sfgts(instr)
      case L.Sfges2 => instructions.sfges(instr)
      case L.Sflts2 => instructions.sflts(instr)
      case L.Sfles2 => instructions.sfles(instr)
      case _ => throw new IllegalInstructionException("Invalid set flag opcode: " + instr.regD)
    }
  }
}
