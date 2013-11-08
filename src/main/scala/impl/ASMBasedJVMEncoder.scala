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

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._

import java.lang.reflect.Method
import java.io.PrintWriter

import scalaz._
import Scalaz._

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.{TraceClassVisitor, CheckClassAdapter}

trait ASMBasedJVMEncoder extends JVMEncoder with IMSyntax {

  sealed case class ASMEncodingState(classes : List[ASMClass] = Nil, freshName : Int = 0)

  type ASMEncoder[A] = State[ASMEncodingState, A]

  sealed case class ASMClass(name : ClassName, writer : ClassWriter) {
    def toByteCode : JVMClass = {
      writer.visitEnd()
      JVMClass(name, writer.toByteArray)
    }
  }

  def encode(ctxt : JVMEncodingCtxt, module : IMModuleClass) : List[JVMClass] = {  
    val run = for (
      klazz <- newClass(module.name) ;
      cw = klazz.writer ;
      _ = cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, module.name.jvmString, null, jvmSuperClass(GeneralObject), null) ;
      elements <- (module.elements map encodeElement(ctxt, klazz)).sequence      
    ) yield elements          
    
    val (state, elements) = run(ASMEncodingState())
    state.classes map (_.toByteCode)
  }

  def newClass(name : ClassName) : ASMEncoder[ASMClass] = {
    for(
      state <- get[ASMEncodingState] ;
      cw = new ClassWriter(ClassWriter.COMPUTE_MAXS) ;
      klazz = ASMClass(name, cw) ;
      _ <- put(state.copy(classes = klazz::state.classes))
    ) yield (klazz)
  }

  def newMethod(cv : ClassVisitor, name : String) : ASMEncoder[MethodVisitor] = {
    val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "()Ljava/lang/Object;", null, null)
    mv.visitCode()
    State({s : ASMEncodingState => (s, mv)})
  }

  /**
   * Expression compilation into method body
   */
  private def bc(mv : MethodVisitor, e : IMCode) : Unit = e match {
    case IMConstInt(v) => {
      mv.visitTypeInsn(NEW, "java/lang/Integer")
      mv.visitInsn(DUP) 
      mv.visitLdcInsn(v)         
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
    }
    case IMConstChar(v) => {
      mv.visitTypeInsn(NEW, "java/lang/Character")
      mv.visitInsn(DUP) 
      mv.visitIntInsn(BIPUSH, v)         
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Character", "<init>", "(C)V")        
    }
    case IMConstString(v) => {
      mv.visitTypeInsn(NEW, "java/lang/String")
      mv.visitInsn(DUP) 
      mv.visitLdcInsn(v)         
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/String", "<init>", "(Ljava/lang/String;)V")
    }
    case IMConstReal(v) => {
      mv.visitTypeInsn(NEW, "java/lang/Double")
      mv.visitInsn(DUP) 
      mv.visitLdcInsn(v)         
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V")
    }
    case IMMatchFail(c) => {
      mv.visitLdcInsn(c) ; 
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/IllegalArgumentException", "<init>", "(Ljava/lang/String;)V") 
      mv.visitInsn(ATHROW);
    }
  }

  /**
    *  Encode a given IM class element into JVM Bytecode using ASM
    */
  def encodeElement(ctxt : JVMEncodingCtxt, current : ASMClass)(element : IMModuleElement) : ASMEncoder[IMModuleElement] = {

    def encClosure(name : ClassName, e : IMCode) = {
      for(
        klazz <- newClass(name) ; 
        cw = klazz.writer ;
        cv = new CheckClassAdapter (cw, false) ;
        _ = cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, name.jvmString, null, jvmSuperClass(ClosureClass), null) ;
        mv <- newMethod(cv, "apply") ;
        _ = mv.visitCode();
        _ = bc(mv, e) ;
        //_ 
        _ = mv.visitInsn(ARETURN);
        _ = mv.visitMaxs(0, 0);
        _ = mv.visitEnd();
        _ = cw.visitEnd()
      ) yield element
    }
    
    def encConstant(name : String, e : IMCode) = {
      for (
        mv <- newMethod(current.writer, name) ;
        _ = mv.visitCode();
        _ = bc(mv, e) ;
        //_ 
        _ = mv.visitInsn(ARETURN);
        _ = mv.visitMaxs(0, 0);
        _ = mv.visitEnd()
      ) yield element
    }

    element match {
      case IMSubClass(IMClosureClass(name, rhs)) => encClosure(name, rhs) 
      case IMConstant(name, value) => encConstant(name, value)
    }
  }

  def decodeToStdOut(classes : List[JVMClass]) = {
    for (klazz <- classes) {
      val cr = new ClassReader(klazz.code)
      cr.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0);
    }
  }

}
