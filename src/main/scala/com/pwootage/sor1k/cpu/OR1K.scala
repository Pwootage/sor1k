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
    val parsed = new Instruction(instr)
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
    I.opcode(i) match {
      case L.J => instructions.j(i)
      case L.Jal => instructions.jal(i)
      case L.Bnf => instructions.bnf(i)
      case L.Bf => instructions.bf(i)
      case L.Movhi => instructions.movhi(i)
      case L.Sys => I.opcode16(i) match {
        case L.Sys2 => instructions.sys(i)
        case L.Trp2 => instructions.trp(i)
        case _ => throw new IllegalInstructionException("Invalid opcode16: " + I.opcode16(i))
      }
      case L.Rfe => instructions.rfe(i)
      case L.Jr => instructions.jr(i)
      case L.Jalr => instructions.jalr(i)
      case L.Nop => instructions.nop(i)
      case L.Lwa => instructions.lwa(i)
      case L.Lwz => instructions.lwz(i)
      case L.Lws => instructions.lws(i)
      case L.Lbz => instructions.lbz(i)
      case L.Lbs => instructions.lbs(i)
      case L.Lhz => instructions.lhz(i)
      case L.Lhs => instructions.lhs(i)
      case L.Addi => instructions.addi(i)
      case L.Addic => instructions.addic(i)
      case L.Andi => instructions.andi(i)
      case L.Ori => instructions.ori(i)
      case L.Xori => instructions.xori(i)
      case L.Muli => instructions.muli(i)
      case L.Mfspr => instructions.mfspr(i)
      case L.Rori => I.opcode2_bit6(i) match {
        case L.Slli2 => instructions.slli(i)
        case L.Srai2 => instructions.srai(i)
        case L.Srli2 => instructions.srli(i)
        case L.Rori2 => instructions.rori(i)
        case _ => throw new IllegalInstructionException("Invalid add opcode2_bitb: " + I.opcode2_bit6(i))
      }
      case L.Sfeqi => I.regD(i) match {
        case L.Sfeqi2 => instructions.sfeqi(i)
        case L.Sfnei2 => instructions.sfnei(i)
        case L.Sfgtui2 => instructions.sfgtui(i)
        case L.Sfgeui2 => instructions.sfgeui(i)
        case L.Sfltui2 => instructions.sfltui(i)
        case L.Sfleui2 => instructions.sfleui(i)
        case L.Sfgtsi2 => instructions.sfgtsi(i)
        case L.Sfgesi2 => instructions.sfgesi(i)
        case L.Sfltsi2 => instructions.sfltsi(i)
        case L.Sflesi2 => instructions.sflesi(i)
        case _ => throw new IllegalInstructionException("Invalid set flag immediate opcode: " + I.regD(i))
      }
      case L.Mtspr => instructions.mtspr(i)
      case L.Swa => instructions.swa(i)
      case L.Sw => instructions.sw(i)
      case L.Sb => instructions.sb(i)
      case L.Sh => instructions.sh(i)
      case L.Add => I.opcode4(i) match {
        case L.Add3 => instructions.add(i)
        case L.Addc3 => instructions.addc(i)
        case L.Sub3 => instructions.sub(i)
        case L.And3 => instructions.and(i)
        case L.Or3 => instructions.or(i)
        case L.Xor3 => instructions.xor(i)
        case L.Mul3 => instructions.mul(i)
        case L.Sll3 => I.opcode2(i) match {
          case L.Sll2 => instructions.sll(i)
          case L.Sra2 => instructions.sra(i)
          case L.Srl2 => instructions.srl(i)
          case L.Ror2 => instructions.ror(i)
        }
        case L.Div3 => instructions.div(i)
        case L.Mulu3 => instructions.mulu(i)
        case L.Exths3 => I.opcode2(i) match {
          case L.Exths2 => instructions.exths(i)
          case L.Extbs2 => instructions.extbs(i)
          case L.Exthz2 => instructions.exthz(i)
          case L.Extbz2 => instructions.extbz(i)
        }
        case L.Divu3 => instructions.divu(i)
        case L.Cmov3 => instructions.cmov(i)
        case _ => throw new IllegalInstructionException("Invalid add opcode4: " + I.opcode4(i))
      }
      //This is actually an opcode but hey, it's the right place
      case L.Sfeq => I.regD(i) match {
        case L.Sfeq2 => instructions.sfeq(i)
        case L.Sfne2 => instructions.sfne(i)
        case L.Sfgtu2 => instructions.sfgtu(i)
        case L.Sfgeu2 => instructions.sfgeu(i)
        case L.Sfltu2 => instructions.sfltu(i)
        case L.Sfl3u2 => instructions.sfleu(i)
        case L.Sfgts2 => instructions.sfgts(i)
        case L.Sfges2 => instructions.sfges(i)
        case L.Sflts2 => instructions.sflts(i)
        case L.Sfles2 => instructions.sfles(i)
        case _ => throw new IllegalInstructionException("Invalid set flag opcode: " + I.regD(i))
      }
      case _ => throw new IllegalInstructionException("Unknown opcode: " + I.opcode(i))
    }
  }
}
