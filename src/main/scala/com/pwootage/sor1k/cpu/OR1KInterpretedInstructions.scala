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

  def mfspr(instr: Instruction): Unit = {
    val spr = (instr.regA | instr.imm16) & 0xFFFF
    reg.gpCtx(instr.regD) = reg.getSPR(spr >> 11, spr & 0x7FF).get
  }

  def movhi(instr: Instruction): Unit = {
    reg.gpCtx(instr.regD) = instr.imm16 << 16
  }

  def mtspr(instr: Instruction): Unit = {
    val spr = (instr.regA | instr.imm16_split) & 0xFFFF
    reg.getSPR(spr >> 11, spr & 0x7FF).set(reg.gpCtx(instr.regB))
  }

  def mul(instr: Instruction): Unit = {
    val regA = reg.gpCtx(instr.regA)
    val regB = reg.gpCtx(instr.regB)
    reg.gpCtx(instr.regD) = regA * regB
    val regDLong = regA.toLong * regB.toLong
    reg.sr.ov = if (regDLong > Int.MaxValue || regDLong < Int.MinValue) 1 else 0
  }

  def muli(instr: Instruction): Unit = {
    val regA = reg.gpCtx(instr.regA)
    val regB = instr.imm16
    reg.gpCtx(instr.regD) = regA * regB
    val regDLong = regA.toLong * regB.toLong
    reg.sr.ov = if (regDLong > Int.MaxValue || regDLong < Int.MinValue) 1 else 0
  }

  def mulu(instr: Instruction): Unit = {
    val regA: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regA).toLong
    val regB: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regB).toLong
    val regD = regA * regB
    reg.gpCtx(instr.regD) = regD.toInt
    reg.sr.ov = if (regD > 0xFFFFFFFFL || regD < 0) 1 else 0
  }

  def nop(instr: Instruction): Unit = {}

  def or(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) | reg.gpCtx(instr.regB)
  }

  def ori(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) | instr.imm16
  }

  def rfe(instr: Instruction): Unit = {
    reg.pc = reg.epcr(reg.sr.cid).get
    reg.sr() = reg.esrr(reg.sr.cid).get
  }

  def ror(instr: Instruction): Unit = {
    val bits = reg.gpCtx(instr.regB) & 0x1F //%32
    val regA = reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = regA >> bits | regA << (32 - bits)
  }

  def rori(instr: Instruction): Unit = {
    val bits = instr.imm5
    val regA = reg.gpCtx(instr.regA)
    reg.gpCtx(instr.regD) = regA >> bits | regA << (32 - bits)
  }

  def sb(instr: Instruction): Unit = {
    val ea = instr.regA + instr.imm16_split.toShort
    mmu.setByte(ea, reg.gpCtx(instr.regB).toByte)
  }

  private def cmps(instr: Instruction, op: (Int, Int) => Boolean) = {
    reg.sr.f = if (op(reg.gpCtx(instr.regA), reg.gpCtx(instr.regB))) 1 else 0
  }

  private def cmpsi(instr: Instruction, op: (Int, Int) => Boolean) = {
    reg.sr.f = if (op(reg.gpCtx(instr.regA), instr.regB.toShort.toInt)) 1 else 0
  }

  private def cmpu(instr: Instruction, op: (Long, Long) => Boolean) = {
    reg.sr.f = if (op(0xFFFFFFFFL & reg.gpCtx(instr.regA), 0xFFFFFFFFL & reg.gpCtx(instr.regB))) 1 else 0
  }

  private def cmpui(instr: Instruction, op: (Long, Long) => Boolean) = {
    reg.sr.f = if (op(0xFFFFFFFFL & reg.gpCtx(instr.regA), 0xFFFFFFFFL & instr.regB.toShort)) 1 else 0
  }

  def sfeq(instr: Instruction) = cmps(instr, _ == _)

  def sfeqi(instr: Instruction) = cmpsi(instr, _ == _)

  def sfges(instr: Instruction) = cmps(instr, _ >= _)

  def sfgesi(instr: Instruction) = cmpsi(instr, _ >= _)

  def sfgeu(instr: Instruction) = cmpu(instr, _ >= _)

  def sfgeui(instr: Instruction) = cmpui(instr, _ >= _)

  def sfgts(instr: Instruction) = cmps(instr, _ > _)

  def sfgtsi(instr: Instruction) = cmpsi(instr, _ > _)

  def sfgtu(instr: Instruction) = cmpu(instr, _ > _)

  def sfgtui(instr: Instruction) = cmpui(instr, _ > _)

  def sfles(instr: Instruction) = cmps(instr, _ <= _)

  def sflesi(instr: Instruction) = cmpsi(instr, _ <= _)

  def sfleu(instr: Instruction) = cmpu(instr, _ <= _)

  def sfleui(instr: Instruction) = cmpui(instr, _ <= _)

  def sflts(instr: Instruction) = cmps(instr, _ < _)

  def sfltsi(instr: Instruction) = cmpsi(instr, _ < _)

  def sfltu(instr: Instruction) = cmpu(instr, _ < _)

  def sfltui(instr: Instruction) = cmpui(instr, _ < _)

  def sfne(instr: Instruction) = cmps(instr, _ != _)

  def sfnei(instr: Instruction) = cmpsi(instr, _ != _)

  def sh(instr: Instruction): Unit = {
    val ea = instr.regA + instr.imm16_split.toShort
    mmu.setShort(ea, reg.gpCtx(instr.regB).toShort)
  }
}
