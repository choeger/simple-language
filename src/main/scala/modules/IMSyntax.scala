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
trait IMSyntax extends PreProcessing {
  
  type IMName = String
 
  /**
    * A field in a JVM - class
    */
  sealed class ClassField private (val name : String, val tipe : IMType)
  
  object ClassField {
    def apply(name : String, tipe : IMType) = new ClassField(escapeIde(name), tipe)
  }

  /**
    * A JVM class name (containing package, main part and sub-class part
    */
  sealed case class ClassName(pkg : List[String], main : String, internal : List[String]) {
    override def toString() = 
      (((main::pkg).reverse.mkString(".")::Nil) ++ internal.reverse).mkString("$")

    def $(name : String) = ClassName(pkg, main, name::internal)

    def jvmString = {
      (((main::pkg).reverse.mkString("/")::Nil) ++ internal.reverse).mkString("$")
    }
  }

  sealed abstract class IMCode
  

  /**
   * A closure creation
   *
   * @parm env The captured environment in '''reverse''' order, i.e.
   *           last(env) = __env[0]
   */
  case class IMCapture(nr : Int, env : List[IMCode]) extends IMCode

  /**
    * A mutually recursive capture
    *
    */
  case class IMLetRec(group : Map[String, Int], fv : List[IMCode], body : IMCode) extends IMCode

  /**
    * A local variable
    */
  case class IMLocalVar(name : IMName) extends IMCode

  /**
    * A closure environment reference
    */
  case class IMClosureVar(nr : Int) extends IMCode

  /**
    * A method argument (numbered for possible currying
    */
  case class IMArgument(nr : Int) extends IMCode

  /**
    * Self reference inside a closure
    */
  case object IMClosureSelf extends IMCode

  /**
    * Application
    */
  case class IMApp(lhs : IMCode, rhs : IMCode) extends IMCode
  
  /**
    * A static variable (reference to a module)
    */
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
  
  sealed abstract class IMType(val desc : String)
  case object TDouble extends IMType("Ljava/lang/Double;")
  case object TInt extends IMType("Ljava/lang/String;"  )
  case object TChar extends IMType("Ljava/lang/Character;")
  case object TString extends IMType("Ljava/lang/String;")
  case object TObject extends IMType("Ljava/lang/Object;")
  case class TClass(name : ClassName) extends IMType(name.jvmString)
  
  sealed abstract class IMClass(val name : ClassName)
  case class IMDataClass(override val name : ClassName, fields : List[ClassField]) extends IMClass(name)
  case class IMModuleClass(override val name : ClassName, elements : List[IMModuleElement]) extends IMClass(name)

  sealed abstract class IMModuleElement
  case class IMClosure(nr : Int, code : IMCode) extends IMModuleElement
  case class IMConstant(name : String, value : IMCode) extends IMModuleElement

}
