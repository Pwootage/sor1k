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

import java.nio.ByteBuffer

import com.pwootage.sor1k.cpu.{OR1KInterpretedInstructions, OR1K}
import com.pwootage.sor1k.memory.MMU
import com.pwootage.sor1k.registers.Registers
import com.pwootage.sor1k.test.BaseSpec

/**
 * Base spec for instruction tests
 */
object InterpretedInstructionFixtures {
  def withCPU(testCode: OR1KInterpretedInstructions => Any) {
    try {
      val reg = new Registers
      val mem = new MMU(reg, ByteBuffer.allocate(1024))
      val or1k = new OR1K(reg, mem)
      val ii = new OR1KInterpretedInstructions(or1k)
      testCode(ii)
    } finally {}
  }
}
