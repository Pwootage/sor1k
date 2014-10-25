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

package com.pwootage.sor1k

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths, Path}

import com.pwootage.sor1k.cpu.OR1K
import com.pwootage.sor1k.memory.MMU
import com.pwootage.sor1k.registers.Registers

/**
 * Main entry-point for VM emulation
 */
object VMMain {
  def main(args: Array[String]) {
    val reg = new Registers
    val mem = new MMU(reg, ByteBuffer.allocate(0x600000)) //6mb of ram
    val cpu = new OR1K(reg, mem)

    val path = Paths.get("/Users/pwootage/vm-shared/openrisc/test/bin/test.bin")

    val binary = Files.readAllBytes(path)

    mem.putByteArray(binary, 0)

    reg.pc = 0x100 //Entry point
    reg.npc = 0x104
    var steps = 0
    while (true) {
      steps += 1
      println(steps, reg.pc.toHexString)
      cpu.executeStep()
      if (reg.pc == 0x2030) {
        println("Jumping. Probably.")
      }
      if (steps % 1000 == 0) {
        println(s"Executed $steps instructions")
      }
    }
  }
}
