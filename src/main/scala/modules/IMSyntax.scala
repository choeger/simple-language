/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.modules

/**
 * JVM-centric intermediate Syntax definition.
 * This requires closure-conversion and pattern matching
 * de-sugaring.
 */
trait IMSyntax {
  
  type IMName = String
 
  sealed case class ClassField(name : String, tipe : IMType)

  sealed case class ClassName(pkg : List[String], main : String, internal : List[String]) {
    override def toString() = (internal.reverse::((main::pkg).mkString("."))::Nil).mkString("$")
    
    def $(name : String) = ClassName(pkg, main, name::internal)

  }

  sealed abstract class IMCode
  
  case class IMCons(klazz : ClassName, args : List[IMCode]) extends IMCode
  case class IMVar(name : IMName) extends IMCode

  case class IMApp(lhs : IMCode, rhs : IMCode) extends IMCode
  
  case class IMStaticAcc(lhs : ClassName, arg : ClassField) extends IMCode
  case class IMAcc(lhs : IMCode, arg : ClassField) extends IMCode

  case class IMLet(n : IMName, d : IMCode, rhs : IMCode) extends IMCode
  case class IMIf(cond : IMCode, th : IMCode, el : IMCode) extends IMCode

  case class IMSimpleMatch(input : IMName, alternatives : List[SimplePattern]) extends IMCode

  sealed case class SimplePattern(className : ClassName, tag : String, 
                                  names : List[IMName], rhs : IMCode)

  case class IMConstInt(value: Int) extends IMCode
  case class IMConstChar(value: Char) extends IMCode
  case class IMConstString(value: String) extends IMCode
  case class IMConstReal(value: Double) extends IMCode
  case class IMMatchFail(constructor : String) extends IMCode
  
  sealed abstract class IMType
  case object TDouble extends IMType
  case object TInt extends IMType
  case object TChar extends IMType
  case object TString extends IMType
  case object TObject extends IMType
  case class TClass(name : ClassName) extends IMType
  
  sealed abstract class IMClass(val name : ClassName)
  case class IMDataClass(override val name : ClassName, fields : List[ClassField]) extends IMClass(name)
  case class IMClosureClass(override val name : ClassName, 
                            closedFields : List[ClassField], 
                            args : Int, 
                            rhs : IMCode) extends IMClass(name)

}
