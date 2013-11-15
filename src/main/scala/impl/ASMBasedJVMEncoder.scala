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

import java.io.PrintWriter

import scalaz._
import Scalaz._

import org.objectweb.asm._
import org.objectweb.asm.commons._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.{TraceClassVisitor, CheckClassAdapter}

trait ASMBasedJVMEncoder extends JVMEncoder with IMSyntax {

  sealed case class ASMEncodingState(current : ASMClass,
                                     classes : List[ASMClass] = Nil,
                                     closures : Map[Int, IMCode] = Map(),
                                     freshName : Int = 0)

  type ASMEncoder[A] = State[ASMEncodingState, A]

  sealed case class ASMClass(name : ClassName, writer : ClassWriter, 
                             dispatch : MethodVisitor) {
    def toByteCode : JVMClass = {
      writer.visitEnd()
      println("Compiling " + name.jvmString)
      JVMClass(name, writer.toByteArray)
    }
  }

  def allocDispatch(nr : Int, e : IMCode) : ASMEncoder[Int] = {
    for {
      state <- get[ASMEncodingState] ;     
      _ <- put(state.copy(closures = state.closures + (nr -> e)))
    } yield nr
  }

  def encode(ctxt : JVMEncodingCtxt, module : IMModuleClass) : List[JVMClass] = {  
    val run = for (
      klazz <- currentModule ;
      cw = klazz.writer ;
      _ = cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, module.name.jvmString, null, jvmSuperClass(GeneralObject), (jvmSuperClass(ClosureClass)::Nil).toArray) ;
      elements <- (module.elements map encodeElement(ctxt)).sequence  ;
      _ <- addDispatchMethod      
    ) yield klazz          
      
    val (state, klazz) = run(newModule(module.name))
    state.classes map (_.toByteCode)
  }

  def newModule(name : ClassName) : ASMEncodingState = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val dispatch = cw.visitMethod(ACC_PRIVATE, "__dispatch", 
                              "(ILjava/lang/Object;)Ljava/lang/Object;", null, null)
    dispatch.visitCode()

    val klazz = ASMClass(name, cw, dispatch)
    addConstructor(klazz)
    ASMEncodingState(klazz, klazz::Nil)
  }

  def addConstructor(klazz : ASMClass) : Unit = {
    klazz.writer.visitField(ACC_FINAL + ACC_PRIVATE, "__index", "I", null, null)
    klazz.writer.visitField(ACC_FINAL + ACC_PRIVATE, "__env", "[Ljava/lang/Object;", null, null)

    val app = Method.getMethod("java.lang.Object apply(java.lang.Object)")
    val appG = new GeneratorAdapter(ACC_PUBLIC, app, null, null, klazz.writer)
    val dispatchM = Method.getMethod("java.lang.Object __dispatch(int,java.lang.Object)")
    
    appG.loadThis
    appG.dup
    appG.getField(Type.getType(klazz.name.jvmString), "__index", Type.INT_TYPE)
    appG.loadArgs
    appG.invokeVirtual(Type.getType(klazz.name.jvmString), dispatchM)
    appG.returnValue
    appG.endMethod
    
    val m = Method.getMethod("void <init> (int, java.lang.Object[])")
    val mg = new GeneratorAdapter(ACC_PUBLIC, m, null, null, klazz.writer)
      
    mg.loadThis
    mg.invokeConstructor(Type.getType("Ljava/lang/Object;"), Method.getMethod("void <init> ()"))

    mg.loadThis
    mg.loadArg(0)    
    mg.putField(Type.getType(klazz.name.jvmString), "__index", Type.INT_TYPE)
      
    mg.loadThis
    mg.loadArg(1)
    mg.putField(Type.getType(klazz.name.jvmString), "__env", Type.getType("[Ljava/lang/Object;"))

    mg.returnValue
      
    mg.visitMaxs(0, 0)
    mg.endMethod
  }

  def newMethod(cv : ClassVisitor, name : String) : ASMEncoder[MethodVisitor] = {
    val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "()Ljava/lang/Object;", null, null)
    mv.visitCode()
    State({s : ASMEncodingState => (s, mv)})
  }

  def currentModule : ASMEncoder[ASMClass] = {
    for (state <- get[ASMEncodingState]) yield state.current
  }

  def dispatchLabels : ASMEncoder[Array[Label]] = {
    for (state <- get[ASMEncodingState]) yield {
      val alabels = new Array[Label](state.closures.size)
      for ((i, e) <- state.closures) {
        alabels.update(i, new Label())
      }
      alabels
    }
  }
  
  def makeDispatch(aLabels : Array[Label])(cls : (Int, IMCode)) : ASMEncoder[Int] = {
    val (i,c) = cls
    println("Compiling Closure " + aLabels(i))
    for (current <- currentModule ;
         dispatch = current.dispatch ;
         _ = dispatch.visitLabel(aLabels(i)) ;
         _ <- bc(dispatch, c) ;
         _ =  dispatch.visitInsn(ARETURN)) yield (i)         
  }

  def compileDispatchs(aLabels : Array[Label]) : ASMEncoder[List[Int]] = {
    for (state <- get[ASMEncodingState] ;
         clss <- state.closures.toList.map(makeDispatch(aLabels)).sequence ;
         _ = println("Compiled: " + aLabels.toList.toString))
      yield clss
  }

  def addDispatchMethod : ASMEncoder[List[Int]] = {
    for (state <- get[ASMEncodingState] ;
         current <- currentModule ;
         aLabels <- dispatchLabels ;
         dflt = new Label ;
         _ = { println(List(aLabels.toList.mkString(", "), current.dispatch, dflt).mkString("; ")) } ;
         _ = if (aLabels.size > 0) {
               current.dispatch.visitVarInsn(ILOAD, 1) 
               current.dispatch.visitTableSwitchInsn(0, aLabels.size-1, dflt, aLabels:_*)               
         } else ();
         /* complete dispatch method */
         compiled <- compileDispatchs(aLabels) ;
         _ = current.dispatch.visitLabel(dflt) ;
         _ = current.dispatch.visitTypeInsn(NEW, "java/lang/RuntimeException") ;
         _ = current.dispatch.visitInsn(DUP) ;
         _ = current.dispatch.visitTypeInsn(NEW, "de/tuberlin/uebb/sl2/runtime/IllegalCodeException") ;
         _ = current.dispatch.visitInsn(DUP) ; 
         _ = current.dispatch.visitVarInsn(ILOAD, 1) ; //get the index
         _ = current.dispatch.visitVarInsn(ALOAD, 2) ; //get the arg
         _ = current.dispatch.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;") ;
         _ = current.dispatch.visitMethodInsn(INVOKESPECIAL, "de/tuberlin/uebb/sl2/runtime/IllegalCodeException", 
                                              "<init>", "(ILjava/lang/Class;)V") ;
         _ = current.dispatch.visitMethodInsn(INVOKESPECIAL, "java/lang/RuntimeException", 
                                              "<init>", "(Ljava/lang/Throwable;)V") ;
         _ = current.dispatch.visitInsn(ATHROW) ;
         _ = current.dispatch.visitMaxs(0, 0);
         _ = current.dispatch.visitEnd()
       ) yield compiled
  }    

  implicit def unit2Enc(f : () => Unit) : ASMEncoder[Unit] = for (_ <- get[ASMEncodingState] ; _ = f() ) yield ()

  def compileCaptureArgs(mv : MethodVisitor, args : List[IMCode]) : List[ASMEncoder[Unit]] = {      val length = args.size
    args.zipWithIndex map {
      case (arg, i) => for (
        _ <- currentModule ;
        _ = mv.visitInsn(DUP) ;
        // turn around, since arguments are in reverse-environment order
        _ = mv.visitLdcInsn(args.size - (i+1)) ;
        _ <- bc(mv, arg) ;
        _ = mv.visitInsn(AASTORE)
      ) yield ()
    }
  }

 /**
  * Expression compilation into method body
  */
  def bc(mv : MethodVisitor, e : IMCode) : ASMEncoder[Unit] = e match {
    case IMCapture(nr, args) => for (
      current <- currentModule ;
      _ = mv.visitTypeInsn(NEW, current.name.jvmString) ;
      _ = mv.visitInsn(DUP) ; 
      _ = mv.visitLdcInsn(nr) ;
      _ = mv.visitLdcInsn(args.size) ;
      _ = mv.visitTypeInsn(ANEWARRAY, "java/lang/Object") ;
      _ <- compileCaptureArgs(mv, args).sequence ;
      _ = mv.visitMethodInsn(INVOKESPECIAL, current.name.jvmString, "<init>", "(I[Ljava/lang/Object;)V")
    ) yield ()
  
      case IMVar(klazz, x) => () => {
        mv.visitVarInsn(ALOAD, 0) ; //this
        mv.visitFieldInsn(GETFIELD, klazz.jvmString, x, "Ljava/lang/Object;")
      }

    case IMIf(c, t, e) => for (
      _ <- bc(mv, c) ;
      tLabel = new Label() ; 
      eLabel = new Label() ;
      
      // evaluate condition
      _ = mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean") ;
      _ = mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z") ;
      _ = mv.visitJumpInsn(IFEQ, eLabel) ;
      
      // then-branch 
      _ <- bc(mv,t) ;
      _ = mv.visitJumpInsn(GOTO, tLabel) ;
          
      // else-branch
      _ = mv.visitLabel(eLabel) ;
      _ <- bc(mv, e) ;
          
      //end-if
      _ = mv.visitLabel(tLabel)      
    ) yield ()

    case IMApp(lhs, rhs) => for (
      _ <- bc(mv, lhs) ;
      _ = mv.visitTypeInsn(CHECKCAST, "de/tuberlin/uebb/sl2/runtime/Closure") ;
      _ <- bc(mv, rhs) ;
      _ = mv.visitMethodInsn(INVOKEINTERFACE, "de/tuberlin/uebb/sl2/runtime/Closure", "apply", 
                             "(Ljava/lang/Object;)Ljava/lang/Object;")
    ) yield ()

    case IMStaticAcc(klazz, field) => () => {
      mv.visitMethodInsn(INVOKESTATIC, klazz.jvmString, field.name, "()" + field.tipe.desc)
    }

    case IMArgument(n) => () => {
      mv.visitVarInsn(ALOAD, 2)
    }

    case IMClosureVar(n) => for {
      current <- currentModule ;
      _ = mv.visitVarInsn(ALOAD, 0) // this
      _ = mv.visitFieldInsn(GETFIELD, current.name.jvmString, "__env", "[Ljava/lang/Object;")
      _ = mv.visitLdcInsn(n) // index
      _ = mv.visitInsn(AALOAD)
    } yield ()

    case IMConstInt(v) => () => {
      mv.visitTypeInsn(NEW, "java/lang/Integer") ;
      mv.visitInsn(DUP) ;
      mv.visitLdcInsn(v) ;        
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V") ;
    }

    case IMConstChar(v) => () => {
      mv.visitTypeInsn(NEW, "java/lang/Character") ;
      mv.visitInsn(DUP) ;
      mv.visitIntInsn(BIPUSH, v) ;
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Character", "<init>", "(C)V") ;
    }

    case IMConstString(v) => () => {
      mv.visitTypeInsn(NEW, "java/lang/String") ;
      mv.visitInsn(DUP) ;
      mv.visitLdcInsn(v) ;        
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/String", "<init>", "(Ljava/lang/String;)V") ;
    }

    case IMConstReal(v) => () => {
      mv.visitTypeInsn(NEW, "java/lang/Double") ;
      mv.visitInsn(DUP) ;
      mv.visitLdcInsn(v)  ;        
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V") ;
    }

    case IMMatchFail(c) => () => {
      mv.visitLdcInsn(c) ; 
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/IllegalArgumentException", "<init>", "(Ljava/lang/String;)V") 
      mv.visitInsn(ATHROW)
    }
  }


  /**
   *  Encode a given IM class element into JVM Bytecode using ASM
   */
  def encodeElement(ctxt : JVMEncodingCtxt)(element : IMModuleElement) : ASMEncoder[IMModuleElement] = {


    def encClosure(nr : Int, e : IMCode) = {
      for(
        l <- allocDispatch(nr, e)
      ) yield element
    }
    
    def encConstant(name : String, e : IMCode) = {
      for (
        current <- currentModule ; 
        cw = current.writer ;
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "()Ljava/lang/Object;", null, null);
        _ = mv.visitCode();
        _ <- bc(mv, e) ;
        _ = mv.visitInsn(ARETURN) ;
        _ = mv.visitMaxs(0,0) ;
        _ = mv.visitEnd()
      ) yield element
    }

    element match {
      case IMClosure(nr, code) => encClosure(nr, code) 
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
