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
import collection.mutable

/**
 * OpenRisc 1000 register set
 */
class Registers {
  /** Current Program Counter */
  var pc = 0
  /** Next Program Counter */
  var npc = pc + 4

  /** General purpose registers (16 sets of 32) */
  val gp = new Array[Int](16 * 32)

  val lr = new SpecialPurposeRegister {
    override def set(v: Int): Unit = gpCtx.update(9, v)

    override def get: Int = gpCtx.apply(9)
  }

  /** Get GPR for current context */
  object gpCtx {
    def apply(ind: Int) = gp(sr.cid * 32 + ind)

    def update(ind: Int, v: Int) = gp.update(sr.cid * 32 + ind, v)
  }

  val spr = new Array[mutable.Map[Int, SpecialPurposeRegister]](32)
  for (i <- 0 until spr.length) spr(i) = mutable.Map[Int, SpecialPurposeRegister]()

  val NullReg = new SpecialPurposeRegister {
    def get = 0

    def set(v: Int) = Unit
  }

  def getSPR(group: Int, reg: Int) = {
    if (group == 0 && reg >= 1024 && reg <= 1535) {
      new SpecialPurposeRegister {
        def set(v: Int): Unit = gp(reg - 1024) = v

        def get: Int = gp(reg - 1024)
      }
    } else {
      spr(group).getOrElse(reg, NullReg)
    }
  }

  /** Version Register */
  val vr = new ReadOnlySPR(0x12000001)


  /** Unit Present Register */
  val upr = new ReadOnlySPR(
    0 |
      (1 << 0) | //UPR
      //      (1 << 3) | //DMMU
      //      (1 << 4) | //IMMU
      (1 << 8) | //PIC
      (1 << 10) //TT
  )

  /** CPU Configuration Register */
  val cpucfgr = new ReadOnlySPR(
    0 |
      (15 << 0) | //15 Shadow GPR's
      (1 << 5) //ORBIS32
  )

  /** Data MMU Configuration Register */
  val dmmucfgr = new ReadOnlySPR(0)

  /** Instruction MMU Configuration Register */
  val immucfgr = new ReadOnlySPR(0)

  /** Debug Configuration Register */
  val dcfgr = new ReadOnlySPR(0)

  /** Version Register 2 */
  val vr2 = new ReadOnlySPR(0xD8000001)

  /** Archetecture Version Register */
  val avr = new ReadOnlySPR(1)

  val epcr = new Array[BasicSPR](16)
  for (i <- 0 until 16) {
    epcr(i) = new BasicSPR
    spr(0)(32 + i) = epcr(i)
  }

  val eear = new Array[BasicSPR](16)
  for (i <- 0 until 16) {
    eear(i) = new BasicSPR
    spr(0)(48 + i) = eear(i)
  }

  val esrr = new Array[BasicSPR](16)
  for (i <- 0 until 16) {
    esrr(i) = new BasicSPR
    spr(0)(64 + i) = esrr(i)
  }

  /** Supervisory Register */
  val sr = new SupervisoryRegister

  val npcSpr  = new SpecialPurposeRegister {
    override def set(v: Int): Unit = Unit

    override def get: Int = npc
  }

  spr(0)(0) = vr
  spr(0)(1) = upr
  spr(0)(2) = cpucfgr
  spr(0)(3) = dmmucfgr
  spr(0)(4) = immucfgr
  spr(0)(7) = dcfgr
  spr(0)(9) = vr2
  spr(0)(10) = avr
  spr(0)(16) = npcSpr
  spr(0)(17) = sr

  def reset(): Unit = {
    sr() = 0 |
      (1 << 15) | //FO
      (1 << 0) //Supervisor Mode
    for (i <- 0 until gp.size) gp(i) = 0
    epcr.foreach(_.set(0))
    eear.foreach(_.set(0))
    esrr.foreach(_.set(0))
  }

  reset() //"reset" to init
}
