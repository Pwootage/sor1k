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
 * Constants for instruction codes as well as instruction decoder
 */
object InstructionCodes {

  object L {
    val J = 0x0
    val Jal = 0x1
    val Bnf = 0x3
    val Bf = 0x4
    val Nop = 0x5
    val Movhi = 0x6
    val Sys = 0x8
    val Sys2 = 0x2000
    val Trp = 0x8
    val Trp2 = 0x2100
    val Rfe = 0x9
    val Jr = 0x11
    val Jalr = 0x12
    val Lwa = 0x1B
    val Lwz = 0x21
    val Lws = 0x22
    val Lbz = 0x23
    val Lbs = 0x24
    val Lhz = 0x25
    val Lhs = 0x26
    val Addi = 0x27
    val Addic = 0x28
    val Andi = 0x29
    val Ori = 0x2A
    val Xori = 0x2B
    val Muli = 0x2C
    val Mfspr = 0x2D

    val Slli = 0x2E
    val Slli2 = 0x0
    val Srli = 0x2E
    val Srli2 = 0x1
    val Srai = 0x2E
    val Srai2 = 0x2
    val Rori = 0x2E
    val Rori2 = 0x3

    val Sfeqi = 0x2F
    val Sfeqi2 = 0x0
    val Sfnei = 0x2F
    val Sfnei2 = 0x1
    val Sfgtui = 0x2F
    val Sfgtui2 = 0x2
    val Sfgeui = 0x2F
    val Sfgeui2 = 0x3
    val Sfltui = 0x2F
    val Sfltui2 = 0x4
    val Sfleui = 0x2F
    val Sfleui2 = 0x5
    val Sfgtsi = 0x2F
    val Sfgtsi2 = 0xA
    val Sfgesi = 0x2F
    val Sfgesi2 = 0xB
    val Sfltsi = 0x2F
    val Sfltsi2 = 0xC
    val Sflesi = 0x2F
    val Sflesi2 = 0xD

    val Mtspr = 0x30
    val Swa = 0x33
    val Sw = 0x35
    val Sb = 0x36
    val Sh = 0x37

    val Add = 0x38
    val Add2 = 0x0
    val Add3 = 0x0
    val Addc = 0x38
    val Addc2 = 0x0
    val Addc3 = 0x1
    val Sub = 0x38
    val Sub2 = 0x0
    val Sub3 = 0x2
    val And = 0x38
    val And2 = 0x0
    val And3 = 0x3
    val Or = 0x38
    val Or2 = 0x0
    val Or3 = 0x4
    val Xor = 0x38
    val Xor2 = 0x0
    val Xor3 = 0x5
    val Mul = 0x38
    val Mul2 = 0x3
    val Mul3 = 0x6

    val Sll = 0x38
    val Sll2 = 0x0
    val Sll3 = 0x8
    val Srl = 0x38
    val Srl2 = 0x1
    val Srl3 = 0x8
    val Sra = 0x38
    val Sra2 = 0x2
    val Sra3 = 0x8
    val Ror = 0x38
    val Ror2 = 0x3
    val Ror3 = 0x8

    val Div = 0x38
    val Div2 = 0x3
    val Div3 = 0x9
    val Mulu = 0x38
    val Mulu2 = 0x3
    val Mulu3 = 0xB

    val Exths = 0x38
    val Exths2 = 0x0
    val Exths3 = 0xC
    val Extbs = 0x38
    val Extbs2 = 0x1
    val Extbs3 = 0xC
    val Exthz = 0x38
    val Exthz2 = 0x2
    val Exthz3 = 0xC
    val Extbz = 0x38
    val Extbz2 = 0x3
    val Extbz3 = 0xC

    val Divu = 0x38
    val Divu2 = 0x3
    val Divu3 = 0xA
    val Cmov = 0x38
    val Cmov2 = 0x0
    val Cmov3 = 0xE

    val Sfeq = 0x39
    val Sfeq2 = 0x0
    val Sfne = 0x39
    val Sfne2 = 0x1
    val Sfgtu = 0x39
    val Sfgtu2 = 0x2
    val Sfgeu = 0x39
    val Sfgeu2 = 0x3
    val Sfltu = 0x39
    val Sfltu2 = 0x4
    val Sfl3u = 0x39
    val Sfl3u2 = 0x5
    val Sfgts = 0x39
    val Sfgts2 = 0xA
    val Sfges = 0x39
    val Sfges2 = 0xB
    val Sflts = 0x39
    val Sflts2 = 0xC
    val Sfles = 0x39
    val Sfles2 = 0xD
  }

  object InterruptVector {
    val Reset = 0x100
    val BusError = 0x200
    val DataPageFault = 0x300
    val InstructionPageFault = 0x400
    val TickTimer = 0x500
    val Alignment = 0x600
    val IllegalInstruction = 0x700
    val ExternalInterrupt = 0x800
    val Range = 0xB00
    val SystemCall = 0xC00
    val FloatingPoint = 0xC00
    val Trap = 0xE00
  }

  object I {
    def opcode(instr: Int) = (instr >>> 26) & 0x3f

    def opcode16(instr: Int) = (instr >>> 16) & 0xFFFF

    def opcode2(instr: Int) = (instr >>> 8) & 0x3

    def opcode2_bit6(instr: Int) = (instr >>> 6) & 0x3

    def opcode4(instr: Int) = (instr >>> 0) & 0xF

    def regD(instr: Int) = (instr >>> 21) & 0x1F

    def regA(instr: Int) = (instr >>> 16) & 0x1F

    def regB(instr: Int) = (instr >>> 11) & 0x1F

    def imm5(instr: Int) = (instr >>> 0) & 0x1F

    def imm6(instr: Int) = (instr >>> 0) & 0x3F

    def imm11(instr: Int) = (instr >>> 0) & 0x7FF

    def imm16(instr: Int) = (instr >>> 0) & 0xFFFF

    def imm16_split(instr: Int) = ((instr >>> 10) & 0xF800) | ((instr >>> 0) & 0x7FF)

    def imm26(instr: Int) = (instr >>> 0) & 0x3FFFFFF
  }

  //these methods *should* be inlined by compiler/jvm
  class Instruction(val instr: Int) {
    def this
    (
      opcode: Int,
      opcode2: Int = 0,
      opcode2_bit6: Int = 0,
      opcode4: Int = 0,
      regD: Int = 0,
      regA: Int = 0,
      regB: Int = 0,
      imm26: Int = 0,
      imm16: Int = 0,
      imm6: Int = 0
      ) = this(
      0 |
        (opcode & 0x3f) << 26 |
        (opcode2 & 0x3) << 8 |
        (opcode2_bit6 & 0x3) << 6 |
        (opcode4 & 0xF) << 0 |
        (regD & 0x1F) << 21 |
        (regA & 0x1F) << 16 |
        (regB & 0x1F) << 11 |
        (imm16 & 0xFFFF) << 0 |
        (imm26 & 0x3FFFFFF) << 0 |
        (imm6 & 0x3F) << 0
    )

    implicit def instrToInt(instr: Instruction) = instr.instr

    def opcode = (instr >>> 26) & 0x3f

    def opcode16 = (instr >>> 16) & 0xFFFF

    def opcode2 = (instr >>> 8) & 0x3

    def opcode2_bit6 = (instr >>> 6) & 0x3

    def opcode4 = (instr >>> 0) & 0xF

    def regD = (instr >>> 21) & 0x1F

    def regA = (instr >>> 16) & 0x1F

    def regB = (instr >>> 11) & 0x1F

    def imm5 = (instr >>> 0) & 0x1F

    def imm6 = (instr >>> 0) & 0x3F

    def imm11 = (instr >>> 0) & 0x7FF

    def imm16 = (instr >>> 0) & 0xFFFF

    def imm16_split = ((instr >>> 10) & 0xF800) | ((instr >>> 0) & 0x7FF)

    def imm26 = (instr >>> 0) & 0x3FFFFFF
  }

}
