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

/**
 * Indicates something has gone wrong with the CPU
 */
class CPUException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

/**
 * Indicates the CPU has entered an invalid state
 */
class IllegalCPUStateException(msg: String, cause: Throwable = null) extends CPUException(msg, cause)

/**
 * Indicates SR has entered an invalid state
 */
class IllegalSRStateException(msg: String, cause: Throwable = null) extends CPUException(msg, cause)

/**
 * Indicates the CPU found an unkonwn instruction
 */
class IllegalInstructionException(msg: String, cause: Throwable = null) extends CPUException(msg, cause)

/**
 * Indicates a device was attempted to be mapped to an invalid address
 */
class IllegalDeviceOffsetException(msg: String, cause: Throwable = null) extends CPUException(msg, cause)

/**
 * Indicates the CPU attempted to access an invalid memory address.
 * Usually triggers a bus error inside the CPU, where it is caught.
 */
class IllegalMemoryAccessException(msg: String, cause: Throwable = null) extends CPUException(msg, cause)