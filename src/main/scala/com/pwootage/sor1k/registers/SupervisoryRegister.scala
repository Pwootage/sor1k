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

package com.pwootage.sor1k.registers

import com.pwootage.sor1k.IllegalSRStateException

/**
 * Describes SR (Supervisory Register) SPR
 */
class SupervisoryRegister extends SpecialPurposeRegister {
  var value = 0

  def get = value

  def set(v: Int): Unit = {
    value = v
    value |= (1 << 15) //Set FO (since it must be set)
    if (lee > 0) throw new IllegalSRStateException("Little Endian mode not supported")
    if (eph > 0) throw new IllegalSRStateException("Exception-pointer high mode not supported")
    if (dsx > 0) throw new IllegalSRStateException("No delay slot implemented")
    if (ice > 0) throw new IllegalSRStateException("Instruction cache not supported")
    if (dce > 0) throw new IllegalSRStateException("Data cache not supported")
  }

  /** Current context ID (0-15) */
  def cid = (value & 0xF0000000) >>> 28

  /**
   * SPRs User Mode Read Access<br />
   * 0 All SPRs are inaccessible in user mode<br />
   * 1 Certain SPRs can be read in user mode
   */
  def sumra = (value & (1 << 16)) >>> 16

  /** Fixed One (always 1)  */
  def fo = (value & (1 << 15)) >>> 15

  /**
   * Exception Prefix High<br/>
   * 0 Exceptions vectors are located in memory area starting at 0x0<br>
   * 1 Exception vectors are located in memory area starting at 0xF0000000
   */
  def eph = (value & (1 << 14)) >>> 14

  /**
   * Delay Slot Exception<br/>
   * 0 EPCR points to instruction not in the delay slot<br/>
   * 1 EPCR points to instruction in delay slot
   **/
  def dsx = (value & (1 << 13)) >>> 13

  /**
   * Overflow flag Exception<br/>
   * 0 Overflow flag does not cause an exception<br/>
   * 1 Overflow flag causes range exception
   */
  def ove = (value & (1 << 12)) >>> 12

  /** Overflow Flag - 1 if overflow, 0 otherwise */
  def ov = (value & (1 << 11)) >>> 11

  /** Carry Out - 1 if carry out, 0 otherwise */
  def cy = (value & (1 << 10)) >>> 10

  /** Flag - Set or unset by sfXX instructions, used by bf/bnf */
  def f = (value & (1 << 9)) >>> 9

  /**
   * CID Enable<br/>
   * 0 CID disabled and shadow registers disabled<br/>
   * 1 CID automatic increment and shadow registers enabled
   **/
  def ce = (value & (1 << 8)) >>> 8

  /**
   * Little Endian Enable<br/>
   * 0 Little Endian (LSB) byte ordering is not enabled<br/>
   * 1 Little Endian (LSB) byte ordering is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  def lee = (value & (1 << 7)) >>> 7

  /**
   * Instruction MMU Enable<br/>
   * 0 Instruction MMU is not enabled<br/>
   * 1 Instruction MMU is enabled
   */
  def ime = (value & (1 << 6)) >>> 6

  /**
   * Data MMU Enable<br/>
   * 0 Data MMU is not enabled<br/>
   * 1 Data MMU is enabled
   */
  def dme = (value & (1 << 5)) >>> 5

  /**
   * Instruction Cache Enable<br/>
   * 0 Instruction Cache is not enabled<br/>
   * 1 Instruction Cache is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  def ice = (value & (1 << 4)) >>> 4

  /**
   * Data Cache Enable<br/>
   * 0 Data Cache is not enabled<br/>
   * 1 Data Cache is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  def dce = (value & (1 << 3)) >>> 3

  /**
   * Interrupt Exception Enabled<br/>
   * 0 Interrupts are not recognized<br/>
   * 1 Interrupts are recognized
   */
  def iee = (value & (1 << 2)) >>> 2

  /**
   * Tick Timer Exception Enabled<br/>
   * 0 Tick Timer Exceptions are not recognized<br/>
   * 1 Tick Timer Exceptions are recognized
   */
  def tee = (value & (1 << 1)) >>> 1

  /**
   * Supervisor Mode<br />
   * 0 Processor is in User Mode<br/>
   * 1 Processor is in Supervisor Mode
   */
  def sm = (value & (1 << 0)) >>> 0
}
