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
 * Describes SR (Supervisory Register) SPR. Implemented as flags which are combined.
 * This is faster in most cases.
 */
class SupervisoryRegister extends SpecialPurposeRegister {
  def get = {
    //TODO: This may be optimizeable?
    0 |
      ((cid & 0xF) << 28) |
      ((sumra & 1) << 16) |
      (1 << 15) | //FO
      ((eph & 1) << 14) |
      ((dsx & 1) << 13) |
      ((ove & 1) << 12) |
      ((ov & 1) << 11) |
      ((cy & 1) << 10) |
      ((f & 1) << 9) |
      ((ce & 1) << 8) |
      ((lee & 1) << 7) |
      ((ime & 1) << 6) |
      ((dme & 1) << 5) |
      ((ice & 1) << 4) |
      ((dce & 1) << 3) |
      ((iee & 1) << 2) |
      ((tee & 1) << 1) |
      ((sm & 1) << 0)
  }

  def set(value: Int): Unit = {
    cid = (value >>> 28) & 0xF
    sumra = (value >>> 16) & 1
    eph = (value >>> 14) & 1
    dsx = (value >>> 13) & 1
    ove = (value >>> 12) & 1
    ov = (value >>> 11) & 1
    cy = (value >>> 10) & 1
    f = (value >>> 9) & 1
    ce = (value >>> 8) & 1
    lee = (value >>> 7) & 1
    ime = (value >>> 6) & 1
    dme = (value >>> 5) & 1
    ice = (value >>> 4) & 1
    dce = (value >>> 3) & 1
    iee = (value >>> 2) & 1
    tee = (value >>> 1) & 1
    sm = (value >>> 0) & 1
    if (lee > 0) throw new IllegalSRStateException("Little Endian mode not supported")
    if (eph > 0) throw new IllegalSRStateException("Exception-pointer high mode not supported")
    if (dsx > 0) throw new IllegalSRStateException("No delay slot implemented")
    //ALthough not supported, I can just ignore them, I belive
    //    if (ice > 0) throw new IllegalSRStateException("Instruction cache not supported")
    //    if (dce > 0) throw new IllegalSRStateException("Data cache not supported")
  }

  /** Current context ID (0-15) */
  var cid = 0

  /**
   * SPRs User Mode Read Access<br />
   * 0 All SPRs are inaccessible in user mode<br />
   * 1 Certain SPRs can be read in user mode
   */
  var sumra = 0

  /**
   * Exception Prefix High<br/>
   * 0 Exceptions vectors are located in memory area starting at 0x0<br>
   * 1 Exception vectors are located in memory area starting at 0xF0000000
   */
  var eph = 0

  /**
   * Delay Slot Exception<br/>
   * 0 EPCR points to instruction not in the delay slot<br/>
   * 1 EPCR points to instruction in delay slot
   **/
  var dsx = 0

  /**
   * Overflow flag Exception<br/>
   * 0 Overflow flag does not cause an exception<br/>
   * 1 Overflow flag causes range exception
   */
  var ove = 0

  /** Overflow Flag - 1 if overflow, 0 otherwise */
  var ov = 0

  /** Carry Out - 1 if carry out, 0 otherwise */
  var cy = 0

  /** Flag - Set or unset by sfXX instructions, used by bf/bnf */
  var f = 0

  /**
   * CID Enable<br/>
   * 0 CID disabled and shadow registers disabled<br/>
   * 1 CID automatic increment and shadow registers enabled
   **/
  var ce = 0

  /**
   * Little Endian Enable<br/>
   * 0 Little Endian (LSB) byte ordering is not enabled<br/>
   * 1 Little Endian (LSB) byte ordering is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  var lee = 0

  /**
   * Instruction MMU Enable<br/>
   * 0 Instruction MMU is not enabled<br/>
   * 1 Instruction MMU is enabled
   */
  var ime = 0

  /**
   * Data MMU Enable<br/>
   * 0 Data MMU is not enabled<br/>
   * 1 Data MMU is enabled
   */
  var dme = 0

  /**
   * Instruction Cache Enable<br/>
   * 0 Instruction Cache is not enabled<br/>
   * 1 Instruction Cache is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  var ice = 0

  /**
   * Data Cache Enable<br/>
   * 0 Data Cache is not enabled<br/>
   * 1 Data Cache is enabled<br/>
   * <br/>
   * <b>NOT IMPLEMENTED, will always be 0</b>
   */
  var dce = 0

  /**
   * Interrupt Exception Enabled<br/>
   * 0 Interrupts are not recognized<br/>
   * 1 Interrupts are recognized
   */
  var iee = 0

  /**
   * Tick Timer Exception Enabled<br/>
   * 0 Tick Timer Exceptions are not recognized<br/>
   * 1 Tick Timer Exceptions are recognized
   */
  var tee = 0

  /**
   * Supervisor Mode<br />
   * 0 Processor is in User Mode<br/>
   * 1 Processor is in Supervisor Mode
   */
  var sm = 1
}
