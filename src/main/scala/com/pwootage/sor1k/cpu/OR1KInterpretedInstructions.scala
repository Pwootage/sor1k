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

  def add(instr: Int) = {
    val regA: Long = reg.gpCtx(instr.regA).toLong
    val regB: Long = reg.gpCtx(instr.regB).toLong
    val regD: Long = regA + regB
    reg.gpCtx(instr.regD) = regD.toInt
    //RegD >> 32 will contain, at most, the carry bit
    reg.sr.cy = (regD >> 32).toInt

    //0 0 1 (overflow)
    //1 1 0 (overflow)
    //Bits the same
    //4 bitwise ands, two equivalence checks, one logical and, if statement comparison
    //    reg.sr.ov = if (
    //      ((regA & LastBit) == (regB & LastBit))
    //        && ((regA & LastBit) != (regD & LastBit))
    //    ) 1 else 0

    //0 0 1 = carry in
    //0 1 0 = carry in
    //1 0 0 = carry in
    //1 1 1 = carry in
    //0 0 0 = no carry in
    //0 1 1 = no carry in
    //1 0 1 = no carry in
    //1 1 0 = no carry in
    //carry in ^ carry out >> 31
    //4 XOR, 2 shifts, one cast
    //Pretty sure this is faster (and is really slick!)
    reg.sr.ov = (
      (regA ^ regB ^ regD) ^ (regD >> 1)
    ).toInt >> 31
  }
}
