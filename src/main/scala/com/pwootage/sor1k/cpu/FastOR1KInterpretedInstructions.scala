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

import com.pwootage.sor1k.IllegalMemoryAccessException

/**
 * Overrides certain implementaitons of instructions to do faster (but less safe) things
 * <br/><br/>
 * Disables overflow for:
 * <ul>
 *   <li>Addition</li>
 *   <li>Subtraction</li>
 *   <li>Multiplication</li>
 * </ul>
 */
class FastOR1KInterpretedInstructions(or1k: OR1K) extends OR1KInterpretedInstructions(or1k){
  import com.pwootage.sor1k.cpu.InstructionCodes.I

  override def add(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) + reg.gp(I.regB(instr))
  }

  override def addc(instr: Int): Unit = {
    reg.gp(I.regD(instr))  = reg.gp(I.regA(instr)) + reg.gp(I.regB(instr))
  }

  override def addi(instr: Int): Unit = {
    reg.gp(I.regD(instr))  = reg.gp(I.regA(instr)) + ((I.imm16(instr) << 16) >> 16)
  }

  override def addic(instr: Int) = {
    reg.gp(I.regD(instr))  = reg.gp(I.regA(instr)) + ((I.imm16(instr) << 16) >> 16)
  }

  override def mul(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) * reg.gp(I.regB(instr))
  }

  override def muli(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) * ((I.imm16(instr) << 16) >> 16)
  }

  override def mulu(instr: Int): Unit = {
    val regA: Long = 0xFFFFFFFFL & reg.gp(I.regA(instr)).toLong
    val regB: Long = 0xFFFFFFFFL & reg.gp(I.regB(instr)).toLong
    reg.gp(I.regD(instr)) = (regA * regB).toInt
  }

  override def sub(instr: Int): Unit = {
    reg.gp(I.regD(instr)) = reg.gp(I.regA(instr)) - reg.gp(I.regB(instr))
  }
}
