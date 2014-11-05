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

import com.pwootage.sor1k.{IllegalMemoryAccessException, IllegalDeviceOffsetException}
import com.pwootage.sor1k.registers.Registers

/**
 * Memory Management Unit for OpenRisc 1000
 */
class MMU(val reg: Registers, buff: ByteBuffer) extends MemoryAccess {
  private val mainMemory = buff.duplicate().order(ByteOrder.BIG_ENDIAN)
  private val devices = new Array[MemoryAccess](0x100)

  def getInstruction(location: Int) = {
    mainMemory.getInt(location)
  }

  /**
   * Registers a device as the device ID specified at 0x9XX00000 where XX is device ID
   */
  def registerDevice(device: MemoryAccess, deviceID: Int) = {
    if (deviceID < 0 || deviceID > 0xFF) {
      throw new IllegalDeviceOffsetException(s"Illegal device offset specified($deviceID), must be 0 to 255")
    }
    devices(deviceID) = device
  }

  def putByteArray(bytes: Array[Byte], off: Int) = {
    buff.position(off)
    buff.put(bytes)
  }

  override def getByte(location: Int) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).getByte(location & 0xFFFFF)
    } else {
      mainMemory.get(location)
    }
  }

  override def getShort(location: Int) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).getShort(location & 0xFFFFF)
    } else {
      mainMemory.getShort(location)
    }
  }

  override def getInt(location: Int) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).getInt(location & 0xFFFFF)
    } else {
      mainMemory.getInt(location)
    }
  }

  override def setByte(location: Int, value: Byte) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).setByte(location & 0xFFFFF, value)
    } else {
      mainMemory.put(location, value)
    }
  }

  override def setShort(location: Int, value: Short) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).setShort(location & 0xFFFFF, value)
    } else {
      mainMemory.putShort(location, value)
    }
  }

  override def setInt(location: Int, value: Int) = {
    if (location < 0) {
      val devid = (location & 0x0FF00000) >> 20
      if (devices(devid) == null) {
        throw new IllegalMemoryAccessException(s"Attempted to access device $devid")
      }
      devices(devid).setInt(location & 0xFFFFF, value)
    } else {
      mainMemory.putInt(location, value)
    }
  }
}
