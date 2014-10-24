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
    val J = (0x0, 0x0, 0x0)
    val Bnf = (0x3, 0x0, 0x0)
    val Bf = (0x4, 0x0, 0x0)

    val Add = (0x38, 0x0, 0x0)
    val Addc = (0x38, 0x0, 0x1)
    val And = (0x38, 0x0, 0x3)
    val Div = (0x38, 0x3, 0x9)

    val Exths = (0x38, 0x0, 0xC)
    val Extbs = (0x38, 0x1, 0xC)
    val Exthz = (0x38, 0x2, 0xC)
    val Extbz = (0x38, 0x3, 0xC)

    val Divu = (0x38, 0x3, 0xA)
    val Cmov = (0x38, 0x0, 0xE)

    val Addi = (0x27, 0x0, 0x0)
    val Addic = (0x28, 0x0, 0x0)
    val Andi = (0x29, 0x0, 0x0)

  }

  //these methods *should* be inlined by compiler/jvm
  implicit class Instruction(val instr: Int) {
    def this
    (
      opcode: Int,
      opcode2: Int = 0,
      opcode4: Int = 0,
      regD: Int = 0,
      regA: Int = 0,
      regB: Int = 0,
      imm26: Int = 0,
      imm16: Int = 0
      ) = this(
      0 |
        (opcode & 0x3f) << 26 |
        (opcode2 & 0x3) << 8 |
        (opcode4 & 0xF) << 0 |
        (regD & 0x1F) << 21 |
        (regA & 0x1F) << 16 |
        (regB & 0x1F) << 11 |
        (imm16 & 0xFFFF) << 0 |
        (imm26 & 0x3FFFFFF) << 0
    )

    implicit def instrToInt(instr: Instruction) = instr.instr

    def opcode = (instr >>> 26) & 0x3f

    def opcode2 = (instr >>> 8) & 0x3

    def opcode4 = (instr >>> 0) & 0xF

    def regD = (instr >>> 21) & 0x1F

    def regA = (instr >>> 16) & 0x1F

    def regB = (instr >>> 11) & 0x1F

    def imm16 = (instr >>> 0) & 0xFFFF

    def imm26 = (instr >>> 0) & 0x3FFFFFF
  }

}
