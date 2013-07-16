/*
 * Copyright (c) 2013, Technische Universität Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.sl2.modules

import scala.language.implicitConversions

/**
  * An interpreter for SL.
  */
trait Interpreter {

  this: Syntax with EnrichedLambdaCalculus with Errors =>

  /**
    * Each SL program maust result in a value, i.e., a value of the built-in types
    * `Int', `Char', and `String' or a constructor value (like `True' or `False').
    */
  sealed abstract class Value
  case class IntVal(int: Int) extends Value
  case class CharVal(char: Char) extends Value
  case class StringVal(str: String) extends Value
  case class ConVal(con: ConVar) extends Value

  /* Implicit conversions from ELC expressions to values */
  implicit def eInt2IntVal(eInt: EInt) = IntVal(eInt.value)
  implicit def eChar2CharVal(eChar: EChar) = CharVal(eChar.value)
  implicit def eStr2StringVal(eStr: EStr) = StringVal(eStr.value)
  implicit def eCon2ConVal(eCon: ECon) = ConVal(eCon.con)

  /* Convert a Scala Boolean to an SL Boolean, i.e., a `True' or `False' constructor */
  implicit def bool2ConVal(bool: Boolean) = if (bool) ConVal("True") else ConVal("False")

  /* Implicit conversions from SL Integers, Characters, and Strings to values */
  implicit def int2IntVal(int: Int) = IntVal(int)
  implicit def char2CharVal(char: Char) = CharVal(char)
  implicit def string2StringVal(str: String) = StringVal(str)


  /**
    * The environment of the interpreter.
    *
    * This map binds evaluated expressions to variables.
    */
  type Environment = Map[EVar, Value]


  /**
    * Evaluate an SL program which has been transformed into the enriched lambda calculus.
    */
  def eval(env: => Environment, expr: ELC): Either[Error, Value]
}
