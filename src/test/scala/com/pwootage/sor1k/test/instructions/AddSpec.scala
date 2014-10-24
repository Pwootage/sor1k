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

package com.pwootage.sor1k.test.instructions

import com.pwootage.sor1k.cpu.InstructionCodes._
import com.pwootage.sor1k.cpu._
import com.pwootage.sor1k.test.BaseSpec
import com.pwootage.sor1k.test.instructions.InterpretedInstructionFixtures._

class AddSpec extends BaseSpec {
  def makeInstruction(opcode: (Int, Int, Int), imm: Int = 0) = new Instruction(
    opcode = opcode._1,
    opcode2 = opcode._2,
    opcode4 = opcode._3,
    regD = 3,
    regA = 1,
    regB = if (imm != 0) 0 else 2,
    imm16 = imm
  ).instr

  def testAdd(a: Int, b: Int = 0, res: Int, carry: Int, overflow: Int, carryIn: Int = 0)(test: OR1K => Any) {
    withCPU { ins =>
      ins.reg.gpCtx(1) = a
      ins.reg.gpCtx(2) = b
      ins.reg.sr.cy = carryIn
      test(ins)
      it("should add correctly") {
        ins.reg.gpCtx(3) should be(res)
      }
      it("should set carry bit correctly") {
        ins.reg.sr.cy should be(carry)
      }
      it("should set overflow bit correctly") {
        ins.reg.sr.ov should be(overflow)
      }
    }
  }

  describe("l.add") {
    describe("basic addition") {
      testAdd(a = 1, b = 1, res = 2, carry = 0, overflow = 0) {
        _.executeInstruction(makeInstruction(L.Add))
      }
    }
    describe("carry with unsigned math") {
      testAdd(a = 0xFFFFFFFF, b = 1, res = 0, carry = 1, overflow = 0) {
        _.executeInstruction(makeInstruction(L.Add))
      }
    }
    describe("overflow with signed math") {
      describe("negatives") {
        testAdd(a = Int.MinValue, b = -1, res = Int.MaxValue, carry = 1, overflow = 1) {
          _.executeInstruction(makeInstruction(L.Add))
        }
      }
      describe("positivies") {
        testAdd(a = Int.MaxValue, b = 1, res = Int.MinValue, carry = 0, overflow = 1) {
          _.executeInstruction(makeInstruction(L.Add))
        }
      }
    }
  }

  describe("l.addc") {
    describe("basic addition") {
      testAdd(a = 1, b = 1, res = 3, carry = 0, overflow = 0, carryIn = 1) {
        _.executeInstruction(makeInstruction(L.Addc))
      }
    }
    describe("carry with unsigned math") {
      testAdd(a = 0xFFFFFFFE, b = 1, res = 0, carry = 1, overflow = 0, carryIn = 1) {
        _.executeInstruction(makeInstruction(L.Addc))
      }
    }
    describe("overflow with signed math") {
      describe("negatives") {
        testAdd(a = Int.MinValue, b = -2, res = Int.MaxValue, carry = 1, overflow = 1, carryIn = 1) {
          _.executeInstruction(makeInstruction(L.Addc))
        }
      }
      describe("positivies") {
        testAdd(a = Int.MaxValue, b = 1, res = Int.MinValue + 1, carry = 0, overflow = 1, carryIn = 1) {
          _.executeInstruction(makeInstruction(L.Addc))
        }
      }
    }
  }

  describe("l.addi") {
    describe("basic addition") {
      testAdd(a = 1, res = 2, carry = 0, overflow = 0) {
        _.executeInstruction(makeInstruction(L.Addi, 1))
      }
    }
    describe("sign-extension subtraction") {
      testAdd(a = 1, res = 0, carry = 1, overflow = 0) {
        _.executeInstruction(makeInstruction(L.Addi, -1))
      }
    }
    describe("carry with unsigned math") {
      testAdd(a = 0xFFFFFFFF, res = 0, carry = 1, overflow = 0) {
        _.executeInstruction(makeInstruction(L.Addi, 1))
      }
    }
    describe("overflow with signed math") {
      describe("negatives") {
        testAdd(a = Int.MinValue, res = Int.MaxValue, carry = 1, overflow = 1) {
          _.executeInstruction(makeInstruction(L.Addi, -1))
        }
      }
      describe("positivies") {
        testAdd(a = Int.MaxValue, res = Int.MinValue, carry = 0, overflow = 1) {
          _.executeInstruction(makeInstruction(L.Addi, 1))
        }
      }
    }
  }

  describe("l.addic") {
    describe("basic addition") {
      testAdd(a = 1, res = 3, carry = 0, overflow = 0, carryIn = 1) {
        _.executeInstruction(makeInstruction(L.Addic, 1))
      }
    }
    describe("sign-extension subtraction") {
      testAdd(a = 2, res = 1, carry = 1, overflow = 0, carryIn = 1) {
        _.executeInstruction(makeInstruction(L.Addic, -2))
      }
    }
    describe("carry with unsigned math") {
      testAdd(a = 0xFFFFFFFF, res = 1, carry = 1, overflow = 0, carryIn = 1) {
        _.executeInstruction(makeInstruction(L.Addic, 1))
      }
    }
    describe("overflow with signed math") {
      describe("negatives") {
        testAdd(a = Int.MinValue, res = Int.MaxValue, carry = 1, overflow = 1, carryIn = 1) {
          _.executeInstruction(makeInstruction(L.Addic, -2))
        }
      }
      describe("positivies") {
        testAdd(a = Int.MaxValue, res = Int.MinValue + 1, carry = 0, overflow = 1, carryIn = 1) {
          _.executeInstruction(makeInstruction(L.Addic, 1))
        }
      }
    }
  }
}
