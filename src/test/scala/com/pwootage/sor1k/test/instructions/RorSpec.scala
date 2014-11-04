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

class RorSpec extends BaseSpec {

  val instr = new Instruction(
    opcode = L.Ror,
    opcode2 =  L.Ror2,
    opcode4 =  L.Ror3,
    regA = 1,
    regB = 2,
    regD = 3
  ).instr

  describe("l.ror") {
    describe("rotate right 0 bits") {
      withCPU { or1k =>
        or1k.reg.gp(1) = 0x12345678
        or1k.reg.gp(2) = 0
        or1k.executeInstruction(instr)
        it("should have the correct value") {
          or1k.reg.gp(3) should be (0x12345678)
        }
      }
    }

    describe("rotate right 32 bits") {
      withCPU { or1k =>
        or1k.reg.gp(1) = 0x12345678
        or1k.reg.gp(2) = 32
        or1k.executeInstruction(instr)
        it("should have the correct value") {
          or1k.reg.gp(3) should be (0x12345678)
        }
      }
    }

    describe("rotate right 1 bit") {
      withCPU { or1k =>
        or1k.reg.gp(1) = 0x12345678
        or1k.reg.gp(2) = 1
        or1k.executeInstruction(instr)
        it("should have the correct value") {
          or1k.reg.gp(3) should be (0x91A2B3C)
        }
      }
    }

    describe("rotate right 16 bits") {
      withCPU { or1k =>
        or1k.reg.gp(1) = 0x12345678
        or1k.reg.gp(2) = 16
        or1k.executeInstruction(instr)
        it("should have the correct value") {
          or1k.reg.gp(3) should be (0x56781234)
        }
      }
    }

    describe("rotate right 31 bits") {
      withCPU { or1k =>
        or1k.reg.gp(1) = 0x12345678
        or1k.reg.gp(2) = 31
        or1k.executeInstruction(instr)
        it("should have the correct value") {
          or1k.reg.gp(3) should be (0x2468ACF0)
        }
      }
    }
  }
}
