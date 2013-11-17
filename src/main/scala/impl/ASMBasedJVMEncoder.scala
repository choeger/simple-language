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
                                     localOffset : Int = 0,
                                     localMax : Int = -1,  
                                     locals : List[String] = Nil)

  type ASMEncoder[A] = State[ASMEncodingState, A]

  sealed case class ASMClass(name : ClassName, writer : ClassWriter, 
                             dispatch : MethodVisitor) {
    def toByteCode : JVMClass = {
      writer.visitEnd()
      JVMClass(name, writer.toByteArray)
    }
  }

  def pushLocal(name:String) : ASMEncoder[Int] = {
    for (state <- get[ASMEncodingState] ;
         _ <- put(state.copy(
           localMax = Math.max(state.localMax, state.locals.size),
           locals = name::state.locals)))
      yield(state.locals.size + state.localOffset)
  }

  /**
    * Creates the bytecode for a new local variable or reuses an older slot
    */
  def introduceLocal(sl : Label, el : Label, mv : MethodVisitor, name : String) : ASMEncoder[Int] = {
    for (max <- maxLocal ;
         idx <- pushLocal(name) ;        
         _ = if (idx > max) { 
          /* Only create variable table entry, if first register hit */
          mv.visitLocalVariable("local_" + idx, "Ljava/lang/Object;", null, sl, el, idx) 
         }
    ) yield idx
  }

  def maxLocal : ASMEncoder[Int] = {
    for (state <- get[ASMEncodingState]) yield state.localMax
  }

  def getLocals : ASMEncoder[(List[String], Int, Int)] = {
    for (state <- get[ASMEncodingState]) yield (state.locals, state.localOffset, state.localMax)
  }

  def setLocals(max : Int, offset : Int, locals : List[String]) = {
    for (state <- get[ASMEncodingState] ;
    _ <- put(state.copy(locals = locals, localOffset = offset, localMax = max))) yield (max)
  }

  def resetLocals(offset : Int) : ASMEncoder[Unit] = {
    for (state <- get[ASMEncodingState] ;
         _ <- put(state.copy(
           localMax = -1,
           localOffset = offset,
           locals = Nil)))
      yield(state.locals.size)
  }

  def popLocal : ASMEncoder[Int] = {
    for (state <- get[ASMEncodingState] ;
         _ <- put(state.copy(locals = state.locals.tail)))
      yield(state.locals.size + state.localOffset)
  }

  def getLocal(name:String) : ASMEncoder[Int] = {
    for (state <- get[ASMEncodingState])
      yield(state.locals.size - state.locals.indexOf(name) + state.localOffset - 1)
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
      _ <- resetLocals(3) ;
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
    for (current <- currentModule ;
         dispatch = current.dispatch ;
         _ = dispatch.visitLabel(aLabels(i)) ;
         _ <- bc(dispatch, c) ;
         _ =  dispatch.visitInsn(ARETURN)) yield (i)         
  }

  def compileDispatchs(aLabels : Array[Label]) : ASMEncoder[List[Int]] = {
    for (state <- get[ASMEncodingState] ;
         clss <- state.closures.toList.map(makeDispatch(aLabels)).sequence)
      yield clss
  }

  def addDispatchMethod : ASMEncoder[List[Int]] = {
    for (state <- get[ASMEncodingState] ;
         current <- currentModule ;
         aLabels <- dispatchLabels ;
         dflt = new Label ;

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

  def setFun(mv: MethodVisitor, sl : Label, el : Label, group : Map[String, Int])
    (x : (String, Int)) : ASMEncoder[Unit] = {
    val (name, index) = x
    val closure = group(name)
    for (current <- currentModule ;
      localIdx <- introduceLocal(sl, el, mv, name) ;
      _ = mv.visitInsn(DUP); // DUP array reference
      _ = mv.visitInsn(DUP); // DUP array reference
      _ = mv.visitTypeInsn(NEW, current.name.jvmString) ; //new closure      
      _ = mv.visitInsn(DUP_X1) ;  //copy ahead of environment
      _ = mv.visitInsn(SWAP) ;  //move environment ahead
      _ = mv.visitLdcInsn(closure) ;
      _ = mv.visitInsn(SWAP) ; // ENV, CLS, CLS, NR, ENV
      _ = mv.visitMethodInsn(INVOKESPECIAL, current.name.jvmString, "<init>", 
        "(I[Ljava/lang/Object;)V") ; // ENV CLS
      _ = mv.visitInsn(DUP) ; // ENV CLS CLS
      _ = mv.visitVarInsn(ASTORE, localIdx) ; //ENV CLS
      _ = mv.visitLdcInsn(index) ; //ENV, CLS, ENTRY
      _ = mv.visitInsn(SWAP) ; // ENV, ENTRY, CLS
      _ = mv.visitInsn(AASTORE)
    ) yield ()
  }

  def setFv(mv : MethodVisitor, offset : Int)(x : (IMCode, Int)) : ASMEncoder[Unit]= {
    val (code : IMCode, index : Int) = x
    for (current <- currentModule ; //expected: ..., ENV
      _ = mv.visitInsn(DUP) ; //..., ENV, ENV
      _ = mv.visitLdcInsn(offset + index) ; // ENV, ENTRY, ENV  
      _ <- bc(mv, code) ; //ENV, ENTRY, VALUE
      _ = mv.visitInsn(AASTORE) 
    ) yield ()
  }

 /**
  * Expression compilation into method body
  */
  def bc(mv : MethodVisitor, e : IMCode) : ASMEncoder[Unit] = e match {
    /*
     * A recursive group of definitions, 
     *  let x1 = \\p1 . ...
     *      ...
     *      xn = \\pn . ...
     * 
     * Compiled into:
     *   Object[] __env = new Object[n + #fv]
     *   x1 = new Closure(numberOf|[e1]|, __env);
     *   env[0] = x1;
     *   ...
     *   xn = new Closure(numberOf|[en]|, __env)
     *   env[n] = xn;
     *   env[n + 1] = |[fv(1)]|;
     *   ...
     */
    case IMLetRec(group, fv, body) => {
      val sl = new Label()
      val el = new Label()
      val names = group.keys.toList.sorted
      val funs = names.zipWithIndex
      val codeFuns = funs map setFun(mv, sl, el, group)

      for (current <- currentModule ;
        _ = mv.visitLdcInsn(funs.size + fv.size) ;
        _ = mv.visitTypeInsn(ANEWARRAY, "java/lang/Object") ;
        _ <- codeFuns.sequence ;
        _ <- fv.zipWithIndex.map(setFv(mv, group.size)).sequence ;
        _ = mv.visitLabel(sl) ;
        _ <- bc(mv, body) ;
        _ = mv.visitLabel(el) ;
        _ <- (names map { _ => popLocal}).sequence
      ) yield ()

    }
    case IMLet(name, d, r) => {
      val sl = new Label()
      val el = new Label()

      for {
        _ <- bc(mv, d) ;
        idx <- introduceLocal(sl, el, mv, name) ;
        _ = mv.visitVarInsn(ASTORE, idx) ;
        _ = mv.visitLabel(sl) ;
        _ <- bc(mv, r) ;
        _ <- popLocal ;
        _ = mv.visitLabel(el) 
      } yield ()
    }

    case IMLocalVar(name) => {
      for {
        idx <- getLocal(name) ;
        _ = mv.visitVarInsn(ALOAD, idx)
      } yield ()
    }

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
        _ <- resetLocals(0) ;
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
