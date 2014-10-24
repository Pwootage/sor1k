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

/**
 * Contains interpreted implementations of OpenRISC instructions
 */
class OR1KInterpretedInstructions(or1k: OR1K) {
  val reg = or1k.reg
  val mmu = or1k.mmu
  val LastBit = 1 << 31

  import InstructionCodes._

  def add(instr: Instruction): Unit = {
    val regA = reg.gpCtx(instr.regA)
    val regB = reg.gpCtx(instr.regB)
    val regD = regA + regB
    reg.gpCtx(instr.regD) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    //4 bitwise ands, two equivalence checks, one logical and, if statement comparison
    //    reg.sr.ov = if (
    //      ((regA & LastBit) == (regB & LastBit))
    //        && ((regA & LastBit) != (regD & LastBit))
    //    ) 1 else 0

    //4 XOR, 2 shifts
    //Pretty sure this is faster (and is really slick!)
    reg.sr.ov = ((regA ^ regB ^ regD) ^ (reg.sr.cy << 31)) >>> 31
    //TODO: Exception handling if those were set
  }

  def addc(instr: Instruction): Unit = {
    val regA = reg.gpCtx(instr.regA)
    val regB = reg.gpCtx(instr.regB)
    val regD = regA + regB + reg.sr.cy
    reg.gpCtx(instr.regD) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ regB ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }

  def addi(instr: Instruction): Unit = {
    val regA = reg.gpCtx(instr.regA)
    val b = instr.imm16.toShort
    val regD = regA + b
    reg.gpCtx(instr.regD) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ b ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }

  def addic(instr: Instruction) = {
    val regA = reg.gpCtx(instr.regA)
    val b = instr.imm16.toShort.toInt
    val regD = regA + b + reg.sr.cy
    reg.gpCtx(instr.regD) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ b ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }

  def and(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & reg.gpCtx(instr.regB)
  }

  def andi(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & instr.imm16
  }

  def bf(instr: Instruction) = if (reg.sr.f > 0) {
    reg.pc = reg.pc + (instr.imm26 << 6) >> 4
    or1k.delaySlot = true
  }

  def bnf(instr: Instruction) = if (reg.sr.f == 0) {
    reg.pc = reg.pc + (instr.imm26 << 6) >> 4
    or1k.delaySlot = true
  }

  def cmov(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(if (reg.sr.f > 0) instr.regA else instr.regB)
  }

  def div(instr: Instruction): Unit = {
    val regB = reg.gpCtx(instr.regB)
    if (regB == 0) {
      reg.sr.ov = 1
    } else {
      reg.sr.ov = 0
      reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) / regB
    }
  }

  def divu(instr: Instruction): Unit = {
    val regB: Long = 0xFFFFFFFFL | reg.gpCtx(instr.regB)
    if (regB == 0) {
      reg.sr.ov = 1
    } else {
      reg.sr.ov = 0
      reg.gpCtx(instr.regD) = ((0xFFFFFFFFL | reg.gpCtx(instr.regA)) / regB).toInt
    }
  }

  def extbs(instr: Instruction): Unit = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA).toByte
  }

  def extbz(instr: Instruction): Unit = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & 0xFF
  }

  def exths(instr: Instruction): Unit = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA).toShort
  }

  def exthz(instr: Instruction): Unit = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & 0xFFFF
  }

  def j(instr: Instruction): Unit = {
    reg.npc = reg.pc + (instr.imm26 << 6) >> 4
    or1k.delaySlot = true
  }

  def jal(instr: Instruction): Unit = {
    reg.npc = reg.pc + (instr.imm26 << 6) >> 4
    or1k.delaySlot = true
    reg.lr() = reg.pc + 8
  }

  def jalr(instr: Instruction): Unit = {
    reg.npc = reg.gpCtx(instr.regB)
    or1k.delaySlot = true
    reg.lr() = reg.pc + 8
  }

  def jr(instr: Instruction): Unit = {
    reg.npc = reg.gpCtx(instr.regB)
    or1k.delaySlot = true
  }

  def lbs(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = mmu.getByte(ea)
  }

  def lbz(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = 0xFF & mmu.getByte(ea).toInt
  }

  def lhs(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = mmu.getShort(ea)
  }

  def lhz(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = 0xFFFF & mmu.getShort(ea).toInt
  }

  //All memory access is atomic in this VM
  def lwa(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = mmu.getInt(ea)
  }

  def lws(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = mmu.getInt(ea)
  }

  def lwz(instr: Instruction): Unit = {
    val ea = instr.imm16.toShort + reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = mmu.getInt(ea)
  }

}
