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

import com.pwootage.sor1k.{IllegalMemoryAccessException, IllegalCPUStateException}

/**
 * Contains interpreted implementations of OpenRISC instructions<br/><br/>
 *
 * TODO: apparently FastOR1K is not broken but this is. Figure out what.
 */
class OR1KInterpretedInstructions(or1k: OR1K) {
  val reg = or1k.reg
  val mmu = or1k.mmu
  val LastBit = 1 << 31

  import InstructionCodes._

  def add(instr: Int): Unit = {
    val regA = reg.gp(I.regA(instr))
    val regB = reg.gp(I.regB(instr))
    val regD = regA + regB
    reg.gp(I.regD(instr)) = regD
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

  def addc(instr: Int): Unit = {
    val regA = reg.gp(I.regA(instr))
    val regB = reg.gp(I.regB(instr))
    val regD = regA + regB + reg.sr.cy
    reg.gp(I.regD(instr)) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ regB ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }

  def addi(instr: Int): Unit = {
    val regA = reg.gp(I.regA(instr))
    val b = (I.imm16(instr) << 16) >> 16
    val regD = regA + b
    reg.gp(I.regD(instr)) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ b ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }

  def addic(instr: Int) = {
    val regA = reg.gp(I.regA(instr))
    val b = (I.imm16(instr) << 16) >> 16
    val regD = regA + b + reg.sr.cy
    reg.gp(I.regD(instr)) = regD
    reg.sr.cy = if ((0xFFFFFFFFL & regD) < (0xFFFFFFFFL & regA)) 1 else 0
    reg.sr.ov = ((regA ^ b ^ regD) ^ (reg.sr.cy << 31)) >>> 31
  }


  def and(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) & reg.gp(I.regB(instr))
  }

  def andi(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) & I.imm16(instr)
  }

  def bf(instr: Int) = if (reg.sr.f > 0) {
    reg.npc = reg.pc + ((I.imm26(instr) << 6) >> 4)
    or1k.delaySlot = true
  }

  def bnf(instr: Int) = if (reg.sr.f == 0) {
    reg.npc = reg.pc + ((I.imm26(instr) << 6) >> 4)
    or1k.delaySlot = true
  }

  def cmov(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(if (reg.sr.f > 0) I.regA(instr) else I.regB(instr))
  }

  def div(instr: Int): Unit = {
    val regB = reg.gp(I.regB(instr))
    if (regB == 0) {
      reg.sr.ov = 1
    } else {
      reg.sr.ov = 0
      reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) / regB
    }
  }

  def divu(instr: Int): Unit = {
    val regB: Long = 0xFFFFFFFFL | reg.gp(I.regB(instr))
    if (regB == 0) {
      reg.sr.ov = 1
    } else {
      reg.sr.ov = 0
      reg.gp(I.regD(instr)) = ((0xFFFFFFFFL | reg.gp(I.regA(instr))) / regB).toInt
    }
  }

  def extbs(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = (reg.gp(I.regA(instr)) << 24) >> 24
  }

  def extbz(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) & 0xFF
  }

  def exths(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = (reg.gp(I.regA(instr)) << 16) >> 16
  }

  def exthz(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) & 0xFFFF
  }

  def j(instr: Int): Unit = {
    reg.npc = reg.pc + ((I.imm26(instr) << 6) >> 4)
    or1k.delaySlot = true
  }

  def jal(instr: Int): Unit = {
    reg.lr.set(reg.pc + 8)
    reg.npc = reg.pc + ((I.imm26(instr) << 6) >> 4)
    or1k.delaySlot = true
  }

  def jalr(instr: Int): Unit = {
    reg.lr.set(reg.pc + 8)
    reg.npc = reg.gp(I.regB(instr))
    or1k.delaySlot = true
  }

  def jr(instr: Int): Unit = {
    reg.npc = reg.gp(I.regB(instr))
    or1k.delaySlot = true
  }

  def lbs(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = mmu.getByte(ea)
  }

  def lbz(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = 0xFF & mmu.getByte(ea).toInt
  }

  def lhs(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = mmu.getShort(ea)
  }

  def lhz(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = 0xFFFF & mmu.getShort(ea).toInt
  }

  //All memory access is atomic in this VM
  def lwa(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = mmu.getInt(ea)
  }

  def lws(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = mmu.getInt(ea)
  }

  def lwz(instr: Int): Unit = {
    val ea = ((I.imm16(instr) << 16) >> 16) + reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = mmu .getInt(ea)
  }

  def mfspr(instr: Int): Unit = {
    if (reg.sr.sm == 0) or1k.except(InterruptVector.IllegalInstruction)
    val spr = (reg.gp(I.regA(instr)) | I.imm16(instr)) & 0xFFFF
    reg.gp(I.regD(instr)) = reg.getSPR(spr >> 11, spr & 0x7FF).get
  }

  def movhi(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = I.imm16(instr) << 16
  }

  def mtspr(instr: Int): Unit = {
    if (reg.sr.sm == 0) or1k.except(InterruptVector.IllegalInstruction)
    val spr = (reg.gp(I.regA(instr)) | I.imm16_split(instr)) & 0xFFFF
    reg.getSPR(spr >> 11, spr & 0x7FF).set(reg.gp(I.regB(instr)))
  }

  def mul(instr: Int): Unit = {
    val regA = reg.gp(I.regA(instr))
    val regB = reg.gp(I.regB(instr))
    reg.gp(I.regD(instr)) = regA * regB
    val regDLong = regA.toLong * regB.toLong
    reg.sr.ov = if (regDLong > Int.MaxValue || regDLong < Int.MinValue) 1 else 0
  }

  def muli(instr: Int): Unit = {
    val regA = reg.gp(I.regA(instr))
    val regB = (I.imm16(instr) << 16) >> 16
    reg.gp(I.regD(instr)) = regA * regB
    val regDLong = regA.toLong * regB.toLong
    reg.sr.ov = if (regDLong > Int.MaxValue || regDLong < Int.MinValue) 1 else 0
  }

  def mulu(instr: Int): Unit = {
    val regA: Long = 0xFFFFFFFFL & reg.gp(I.regA(instr)).toLong
    val regB: Long = 0xFFFFFFFFL & reg.gp(I.regB(instr)).toLong
    val regD = regA * regB
    reg.gp(I.regD(instr)) = regD.toInt
    reg.sr.ov = if (regD > 0xFFFFFFFFL || regD < 0) 1 else 0
  }

  def nop(instr: Int): Unit = {}

  def or(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) | reg.gp(I.regB(instr))
  }

  def ori(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) | I.imm16(instr)
  }

  def rfe(instr: Int): Unit = {
    reg.npc = reg.epcr.get
    reg.sr.set(reg.esrr.get)
  }

  def ror(instr: Int): Unit = {
    val bits = reg.gp(I.regB(instr)) & 0x1F //%32
    val regA = reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = regA >> bits | regA << (32 - bits)
  }

  def rori(instr: Int): Unit = {
    val bits = I.imm5(instr)
    val regA = reg.gp(I.regA(instr))
    reg.gp(I.regD(instr)) = regA >> bits | regA << (32 - bits)
  }

  def sb(instr: Int): Unit = {
    val ea = reg.gp(I.regA(instr)) + ((I.imm16_split(instr) << 16) >> 16)
    mmu.setByte(ea, reg.gp(I.regB(instr)).toByte)
  }

  private def cmps(instr: Int, op: (Int, Int) => Boolean) = {
    reg.sr.f = if (op(reg.gp(I.regA(instr)), reg.gp(I.regB(instr)))) 1 else 0
  }

  private def cmpsi(instr: Int, op: (Int, Int) => Boolean) = {
    reg.sr.f = if (op(reg.gp(I.regA(instr)), (I.imm16(instr) << 16) >> 16)) 1 else 0
  }

  private def cmpu(instr: Int, op: (Long, Long) => Boolean) = {
    reg.sr.f = if (op(0xFFFFFFFFL & reg.gp(I.regA(instr)), 0xFFFFFFFFL & reg.gp(I.regB(instr)))) 1 else 0
  }

  private def cmpui(instr: Int, op: (Long, Long) => Boolean) = {
    reg.sr.f = if (op(0xFFFFFFFFL & reg.gp(I.regA(instr)), 0xFFFFFFFFL & ((I.imm16(instr) << 16) >> 16))) 1 else 0
  }

  def sfeq(instr: Int) = cmps(instr, _ == _)

  def sfeqi(instr: Int) = cmpsi(instr, _ == _)

  def sfges(instr: Int) = cmps(instr, _ >= _)

  def sfgesi(instr: Int) = cmpsi(instr, _ >= _)

  def sfgeu(instr: Int) = cmpu(instr, _ >= _)

  def sfgeui(instr: Int) = cmpui(instr, _ >= _)

  def sfgts(instr: Int) = cmps(instr, _ > _)

  def sfgtsi(instr: Int) = cmpsi(instr, _ > _)

  def sfgtu(instr: Int) = cmpu(instr, _ > _)

  def sfgtui(instr: Int) = cmpui(instr, _ > _)

  def sfles(instr: Int) = cmps(instr, _ <= _)

  def sflesi(instr: Int) = cmpsi(instr, _ <= _)

  def sfleu(instr: Int) = cmpu(instr, _ <= _)

  def sfleui(instr: Int) = cmpui(instr, _ <= _)

  def sflts(instr: Int) = cmps(instr, _ < _)

  def sfltsi(instr: Int) = cmpsi(instr, _ < _)

  def sfltu(instr: Int) = cmpu(instr, _ < _)

  def sfltui(instr: Int) = cmpui(instr, _ < _)

  def sfne(instr: Int) = cmps(instr, _ != _)

  def sfnei(instr: Int) = cmpsi(instr, _ != _)

  def sh(instr: Int): Unit = {
    val ea = reg.gp(I.regA(instr)) + ((I.imm16_split(instr) << 16) >> 16)
    mmu.setShort(ea, reg.gp(I.regB(instr)).toShort)
  }

  def sll(instr: Int): Unit = {
    val shift = reg.gp(I.regB(instr)) & 0x1F //%32
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) << shift
  }

  def slli(instr: Int): Unit = {
    val shift = I.imm5(instr)
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) << shift
  }

  def sra(instr: Int): Unit = {
    val shift = reg.gp(I.regB(instr)) & 0x1F //%32
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) >> shift
  }

  def srai(instr: Int): Unit = {
    val shift = I.imm5(instr)
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) >> shift
  }

  def srl(instr: Int): Unit = {
    val shift = reg.gp(I.regB(instr)) & 0x1F //%32
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) >>> shift
  }

  def srli(instr: Int): Unit = {
    val shift = I.imm5(instr)
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) >>> shift
  }

  def sub(instr: Int): Unit = {
    reg.gp(I.regB(instr)) = -reg.gp(I.regB(instr))
    add(instr)
    reg.gp(I.regB(instr)) = -reg.gp(I.regB(instr))
  }

  def sw(instr: Int): Unit = {
    val ea = reg.gp(I.regA(instr)) + ((I.imm16_split(instr) << 16) >> 16)
    try mmu.setInt(ea, reg.gp(I.regB(instr))) catch {
      case e: IndexOutOfBoundsException => throw new IllegalMemoryAccessException(s"Attempted to access location ${ea.formatted("%08x")}", e)
    }
  }

  def swa(instr: Int): Unit = {
    val ea = reg.gp(I.regA(instr)) + ((I.imm16_split(instr) << 16) >> 16)
    mmu.setInt(ea, reg.gp(I.regB(instr)))
  }

  def sys(instr: Int): Unit = {
    or1k.except(InterruptVector.SystemCall, 4)
  }

  def trp(instr: Int): Unit = {
    I.imm16(instr) match {
      case 0x1 => {
        nop(0)
      }
      case 0x2 => print("0x" + reg.gp(3).toHexString)
      case 0x3 => print(String.valueOf(reg.gp(3).toChar))
      case 0x10 => {
        val memStart = reg.gp(3)
        val cols = 4 * 8 //16 bytes
        val rows = 16
        for (i <- 0 until rows) {
          print("0x" + (memStart + i * cols).formatted("%08x") + ": ")
          for (j <- 0 until cols) {
            if (j % 2 == 0) print(" ")
            if (j % 4 == 0) print(" ")
            print(mmu.getByte(memStart + i * cols + j).formatted("%02x"))
          }
          println()
        }
      }
      case 0xFF => throw new IllegalCPUStateException("Got kill command!");
      case _ => if (reg.sr.sm == 0) or1k.except(InterruptVector.Trap, 4)
    }
  }

  def xor(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) ^ reg.gp(I.regB(instr))
  }

  def xori(instr: Int) = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) ^ ((I.imm16(instr) << 16) >> 16)
  }
}
