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

package com.pwootage.sor1k.memory

import java.nio.{ByteOrder, ByteBuffer}

import com.pwootage.sor1k.registers.Registers

/**
 * Memory Management Unit for OpenRisc 1000
 */
class MMU(val reg: Registers, buff: ByteBuffer) {
  private val mainMemory = buff.duplicate().order(ByteOrder.BIG_ENDIAN)

  def getInstruction(location: Int) = {
    mainMemory.getInt(location)
  }

  def getByte(location: Int) = {
    mainMemory.get(location)
  }

  def getShort(location: Int) = {
    mainMemory.getShort(location)
  }

  def getInt(location: Int) = {
    mainMemory.getInt(location)
  }

  def setByte(location: Int, value: Byte) = {
    mainMemory.put(location, value)
  }

  def setShort(location: Int, value: Short) = {
    mainMemory.putShort(location, value)
  }

  def setInt(location: Int, value: Int) = {
    mainMemory.putInt(location, value)
  }
}
