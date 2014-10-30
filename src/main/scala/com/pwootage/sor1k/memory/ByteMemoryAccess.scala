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

/**
 * Memory access with proxy methods for storing things larger than bytes, to make it a bit easier
 */
abstract class ByteMemoryAccess extends MemoryAccess {
  override def getShort(location: Int): Short = {
    (getByte(location + 1) | (getByte(location) << 8)).toShort
  }

  override def getInt(location: Int): Int = {
    getByte(location + 3) | (getByte(location + 2) << 8) |
      getByte(location + 1) << 16 | (getByte(location) << 24)
  }

  override def setShort(location: Int, value: Short): Unit = {
    setByte(location + 1, (value & 0xFF).toByte)
    setByte(location, ((value >> 8) & 0xFF).toByte)
  }

  override def setInt(location: Int, value: Int): Unit = {
    setByte(location + 3, (value & 0xFF).toByte)
    setByte(location + 2, ((value >> 8) & 0xFF).toByte)
    setByte(location + 1, ((value >> 16) & 0xFF).toByte)
    setByte(location, ((value >> 24) & 0xFF).toByte)
  }
}
