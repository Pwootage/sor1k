package com.pwootage.sor1k.registers

import com.pwootage.sor1k.IllegalSRStateException

/**
 * Basic read-only SPR
 */
class ReadOnlySPR(val value: Int) extends SpecialPurposeRegister {
  def get = value

  def set(v: Int) = throw new IllegalSRStateException("Attempted to write to read-only SPR")
}
