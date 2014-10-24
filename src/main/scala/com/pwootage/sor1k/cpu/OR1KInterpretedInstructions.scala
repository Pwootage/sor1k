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
    val regA: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regA)
    val regB: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regB)
    val regD: Long = regA + regB
    reg.gpCtx(instr.regD) = regD.toInt
    reg.sr.cy = (regD >> 32).toInt
    //4 bitwise ands, two equivalence checks, one logical and, if statement comparison
    //    reg.sr.ov = if (
    //      ((regA & LastBit) == (regB & LastBit))
    //        && ((regA & LastBit) != (regD & LastBit))
    //    ) 1 else 0

    //4 XOR, 2 shifts, one cast
    //Pretty sure this is faster (and is really slick!)
    reg.sr.ov = ((regA ^ regB ^ regD) ^ (regD >> 1)).toInt >>> 31
    //TODO: Exception handling if those were set
  }

  def addc(instr: Instruction): Unit = {
    val regA: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regA)
    val regB: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regB)
    val regD: Long = regA + regB + reg.sr.cy
    reg.gpCtx(instr.regD) = regD.toInt
    reg.sr.cy = (regD >> 32).toInt
    reg.sr.ov = ((regA ^ regB ^ regD) ^ (regD >> 1)).toInt >>> 31
  }

  def addi(instr: Instruction): Unit = {
    val regA: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regA)
    val b: Long = 0xFFFFFFFFL & instr.imm16.toShort
    val regD: Long = regA + b
    reg.gpCtx(instr.regD) = regD.toInt
    reg.sr.cy = (regD >> 32).toInt
    reg.sr.ov = ((regA ^ b ^ regD) ^ (regD >> 1)).toInt >>> 31
  }

  def addic(instr: Instruction) = {
    val regA: Long = 0xFFFFFFFFL & reg.gpCtx(instr.regA)
    val b: Long = 0xFFFFFFFFL & instr.imm16.toShort
    val regD: Long = regA + b + reg.sr.cy
    reg.gpCtx(instr.regD) = regD.toInt
    reg.sr.cy = (regD >> 32).toInt
    reg.sr.ov = ((regA ^ b ^ regD) ^ (regD >> 1)).toInt >>> 31
  }

  def and(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & reg.gpCtx(instr.regB)
  }

  def andi(instr: Instruction) = {
    reg.gpCtx(instr.regD) = reg.gpCtx(instr.regA) & instr.imm16
  }

  def bf(instr: Instruction) = if (reg.sr.f > 0) {
    val imm = instr.imm26 << 2
    reg.pc = reg.pc + (if ((imm & 0x8000000) > 0) imm | 0xF0000000 else imm)
  }

  def bnf(instr: Instruction) = if (reg.sr.f == 0) {
    val imm = instr.imm26 << 2
    reg.pc = reg.pc + (if ((imm & 0x8000000) > 0) imm | 0xF0000000 else imm)
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
    val imm = instr.imm26 << 2
    reg.pc = reg.pc + (if ((imm & 0x8000000) > 0) imm | 0xF0000000 else imm)
  }
}
