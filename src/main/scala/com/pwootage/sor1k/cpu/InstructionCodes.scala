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
    val Add = (0x38, 0x0, 0x0)
    val Addc = (0x38, 0x0, 0x1)
  }

  //these methods *should* be inlined by compiler/jvm
  implicit class Instruction(instr: Int) {
    def opcode = (instr >>> 26) & 0x3f

    def opcode2 = (instr >>> 8) & 0x3

    def opcode4 = (instr >>> 0) & 0xF

    def regD = (instr >>> 21) & 0x1F

    def regA = (instr >>> 16) & 0x1F

    def regB = (instr >>> 11) & 0x1F
  }
}
