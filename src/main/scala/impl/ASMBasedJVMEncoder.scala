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

  sealed case class ASMEncodingState(classes : List[ClassWriter] = Nil, freshName : Int = 0)

  type ASMEncoder[A] = State[ASMEncodingState, A]

  /**
    *  Encode a given IM expression into JVM Bytecode using ASM
    *  This method returns a whole class
    */
  def encode(ctxt : JVMEncodingCtxt, expr : IMCode) : Array[Byte] = {

    def newClass : ASMEncoder[ClassWriter] = {
      for(
        state <- get[ASMEncodingState] ;
        cw = new ClassWriter(ClassWriter.COMPUTE_MAXS) ;
        _ <- put(state.copy(classes = cw::state.classes))
      ) yield (cw)
    }

    def newStaticMethod(cv : ClassVisitor, name : String) : ASMEncoder[MethodVisitor] = {
      val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "()Ljava/lang/Object;", null, null)
      mv.visitCode()
      State({s : ASMEncodingState => (s, mv)})
    }

    def enc(e : IMCode) : ASMEncoder[Array[Byte]] = {
      for(
        cw <- newClass ;
        cv = new CheckClassAdapter (cw, false) ;
        _ = cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, ctxt.fullName, null, ctxt.superType, null) ;
        mv <- newStaticMethod(cv, "eval") ;
        _ = mv.visitCode();
        _ = bc(mv, e) ;
        //_ 
        _ = mv.visitInsn(ACONST_NULL);
        _ = mv.visitInsn(ARETURN);
        _ = mv.visitMaxs(0, 0);
        _ = mv.visitEnd();
        _ = cw.visitEnd();
        bytes = cw.toByteArray()
      ) yield bytes
    }

    /**
      * Expression compilation into method body
      */
    def bc(mv : MethodVisitor, e : IMCode) : Unit = e match {
      case IMConstInt(v) => mv.visitLdcInsn(v)
      case IMConstChar(v) => mv.visitLdcInsn(v)
      case IMConstString(v) => mv.visitLdcInsn(v)
      case IMConstReal(v) => mv.visitLdcInsn(v)
      case IMMatchFail(c) => {
        mv.visitLdcInsn(c) ; 
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/IllegalArgumentException", "<init>", "(Ljava/lang/String;)V") ;
        mv.visitInsn(ATHROW);
      }
      case _ =>
    }

    val (state, bytes) = enc(expr)(ASMEncodingState())
    bytes
  }

  def decodeToStdOut(bytes : Array[Byte]) = {
    val cr = new ClassReader(bytes)
    cr.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0);
  }

}
