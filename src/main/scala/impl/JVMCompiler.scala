
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
import de.tuberlin.uebb.sl2.runtime

import scala.collection.mutable 

import de.tuberlin.uebb.sl2.modules.Syntax._

trait JVMCompiler extends ModuleCompiler with ExprCompiler 
with SimpleIMEncoder with ASMBasedJVMEncoder with ByteCodeInterpreter 
with Syntax with SyntaxTraversal with IMSyntax with JVMEncoder with ModuleResolverImpl
with ProgramCheckerImpl 
with ModuleNormalizerImpl with ModuleContextImpl with ModuleLinearization
with SignatureJsonSerializer 
with Errors with Configs
with Lexic with EnrichedLambdaCalculus
with Type with NameSupply
with Context with Substitution
with Unification with GraphImpl[VarFirstClass]
with LetRecSplitter with DTCheckerImpl
with FDCheckerImpl with TypeCheckerImpl
with ContextBuilder {   

  this : Parser =>
    
  /**
   * @inheritdoc
   */
  def compileExpr(className : ClassName, name : String, expr : String) : Either[Error, List[JVMClass]] = {
    for (e <- parseExpr(expr).right ;
         mod <- doCompileModule(className, buildModule(name, e)).right
    ) yield mod
  }

  def buildModule(name : String, e : Expr) = {
    Program(Nil, Map(), Map(), Nil)
  }

  /**
   * @inheritdoc
   */
  def compileModule(className : ClassName, p : String) : Either[Error, List[JVMClass]] = {
    for(// parse the module
        prog <- parseAst(p).right ; 
        classes <- doCompileModule(className, prog.asInstanceOf[Program]).right
    ) yield classes
  }

  def doCompileModule(className : ClassName, p : Program) = {
    for (
        // check and load dependencies
        dependencies <- inferDependencies(p, config).right;
      
        // type check the program
        _ <- checkProgram(p, normalizeModules(dependencies)).right
    ) yield { 
      val ctxt = context(p, dependencies)
      val main = encode(IMEncodingEnv(className, Set(), Map(), ctxt), p)
      encode(JVMEncodingCtxt(), main)
    }
  }

}
