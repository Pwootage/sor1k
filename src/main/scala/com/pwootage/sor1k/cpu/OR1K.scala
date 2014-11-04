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
    ins.opcode match {
      case L.J => instructions.j(ins)
      case L.Jal => instructions.jal(ins)
      case L.Bnf => instructions.bnf(ins)
      case L.Bf => instructions.bf(ins)
      case L.Movhi => instructions.movhi(ins)
      case L.Sys => ins.opcode16 match {
        case L.Sys2 => instructions.sys(ins)
        case L.Trp2 => instructions.trp(ins)
        case _ => throw new IllegalInstructionException("Invalid opcode16: " + ins.opcode16)
      }
      case L.Rfe => instructions.rfe(ins)
      case L.Jr => instructions.jr(ins)
      case L.Jalr => instructions.jalr(ins)
      case L.Nop => instructions.nop(ins)
      case L.Lwa => instructions.lwa(ins)
      case L.Lwz => instructions.lwz(ins)
      case L.Lws => instructions.lws(ins)
      case L.Lbz => instructions.lbz(ins)
      case L.Lbs => instructions.lbs(ins)
      case L.Lhz => instructions.lhz(ins)
      case L.Lhs => instructions.lhs(ins)
      case L.Addi => instructions.addi(ins)
      case L.Addic => instructions.addic(ins)
      case L.Andi => instructions.andi(ins)
      case L.Ori => instructions.ori(ins)
      case L.Xori => instructions.xori(ins)
      case L.Muli => instructions.muli(ins)
      case L.Mfspr => instructions.mfspr(ins)
      case L.Rori => ins.opcode2_bit6 match {
        case L.Slli2 => instructions.slli(ins)
        case L.Srai2 => instructions.srai(ins)
        case L.Srli2 => instructions.srli(ins)
        case L.Rori2 => instructions.rori(ins)
        case _ => throw new IllegalInstructionException("Invalid add opcode2_bitb: " + ins.opcode2_bit6)
      }
      case L.Sfeqi => ins.regD match {
        case L.Sfeqi2 => instructions.sfeqi(ins)
        case L.Sfnei2 => instructions.sfnei(ins)
        case L.Sfgtui2 => instructions.sfgtui(ins)
        case L.Sfgeui2 => instructions.sfgeui(ins)
        case L.Sfltui2 => instructions.sfltui(ins)
        case L.Sfleui2 => instructions.sfleui(ins)
        case L.Sfgtsi2 => instructions.sfgtsi(ins)
        case L.Sfgesi2 => instructions.sfgesi(ins)
        case L.Sfltsi2 => instructions.sfltsi(ins)
        case L.Sflesi2 => instructions.sflesi(ins)
        case _ => throw new IllegalInstructionException("Invalid set flag immediate opcode: " + ins.regD)
      }
      case L.Mtspr => instructions.mtspr(ins)
      case L.Swa => instructions.swa(ins)
      case L.Sw => instructions.sw(ins)
      case L.Sb => instructions.sb(ins)
      case L.Sh => instructions.sh(ins)
      case L.Add => ins.opcode4 match {
        case L.Add3 => instructions.add(ins)
        case L.Addc3 => instructions.addc(ins)
        case L.Sub3 => instructions.sub(ins)
        case L.And3 => instructions.and(ins)
        case L.Or3 => instructions.or(ins)
        case L.Xor3 => instructions.xor(ins)
        case L.Mul3 => instructions.mul(ins)
        case L.Sll3 => ins.opcode2 match {
          case L.Sll2 => instructions.sll(ins)
          case L.Sra2 => instructions.sra(ins)
          case L.Srl2 => instructions.srl(ins)
          case L.Ror2 => instructions.ror(ins)
        }
        case L.Div3 => instructions.div(ins)
        case L.Mulu3 => instructions.mulu(ins)
        case L.Exths3 => ins.opcode2 match {
          case L.Exths2 => instructions.exths(ins)
          case L.Extbs2 => instructions.extbs(ins)
          case L.Exthz2 => instructions.exthz(ins)
          case L.Extbz2 => instructions.extbz(ins)
        }
        case L.Divu3 => instructions.divu(ins)
        case L.Cmov3 => instructions.cmov(ins)
        case _ => throw new IllegalInstructionException("Invalid add opcode4: " + ins.opcode4)
      }
      //This is actually an opcode but hey, it's the right place
      case L.Sfeq => ins.regD match {
        case L.Sfeq2 => instructions.sfeq(ins)
        case L.Sfne2 => instructions.sfne(ins)
        case L.Sfgtu2 => instructions.sfgtu(ins)
        case L.Sfgeu2 => instructions.sfgeu(ins)
        case L.Sfltu2 => instructions.sfltu(ins)
        case L.Sfl3u2 => instructions.sfleu(ins)
        case L.Sfgts2 => instructions.sfgts(ins)
        case L.Sfges2 => instructions.sfges(ins)
        case L.Sflts2 => instructions.sflts(ins)
        case L.Sfles2 => instructions.sfles(ins)
        case _ => throw new IllegalInstructionException("Invalid set flag opcode: " + ins.regD)
      }
      case _ => throw new IllegalInstructionException("Unknown opcode: " + ins.opcode)
    }
  }
}
