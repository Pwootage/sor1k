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

package com.pwootage.sor1k.devices

import com.pwootage.sor1k.IllegalMemoryAccessException
import com.pwootage.sor1k.cpu.{InstructionCodes, OR1K}
import com.pwootage.sor1k.memory.{ByteMemoryAccess, MemoryAccess}

/**
 * Basic x86-style text mode buffer.
 *
 * 0x00000 - Beginning of frame buffer
 *
 * 0xF0000 - Screen Width
 * 0xF0001 - Screen Height
 *
 */
class VGATextModeBuffer(or1k: OR1K) extends ByteMemoryAccess {
  val buffer = new Array[Byte](160*50*2) //Max buffer size. Actual buffer size will vary
  /** Sizes (OC): 50×16/80×25/160×50 */
  var size = (50.toByte, 16.toByte)

  override def getByte(location: Int): Byte = {
    if (location < 0) {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location < 0)")
    } else if (location < buffer.length) {
      buffer(location)
    } else if (location < 0xF0000) {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location > buffer, < registers)")
    } else if (location == 0xF0000) {
      size._1
    } else if (location == 0xF0001) {
      size._2
    } else {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location > registers)")
    }
  }

  override def setByte(location: Int, value: Byte): Unit = {
    if (location < 0) {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location < 0)")
    } else if (location < buffer.length) {
      buffer(location) = value
    } else if (location < 0xF0000) {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location > buffer, < registers)")
    } else if (location == 0xF0000) {
      size = (value, size._2)
    } else if (location == 0xF0001) {
      size = (size._1, value)
    } else {
      throw new IllegalMemoryAccessException(s"Attempted to access invalid VGA address ($location > registers)")
    }
  }
}
