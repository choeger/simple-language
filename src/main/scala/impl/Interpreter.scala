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

import de.tuberlin.uebb.sl2.modules.Syntax._

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.impl._

import org.kiama.util.{REPL}
import org.joda.time.DateTime

import scalax.file.{Path, FileSystem}
import scalax.file.ImplicitConverters._

import scala.language.reflectiveCalls // used for asFile

object Interpreter extends CombinatorParser  with REPL
with JVMCompiler with JVMClassInterpreter
{   

  sealed abstract class Mode
  case object ExprMode extends Mode
  case object ModMode extends Mode

  object SwitchExprMode {
    def unapply(x : String) : Boolean = {
      x == ":expr"
    }
  }

  object SwitchModuleMode {
    def unapply(x : String) : Boolean = {
      x == ":mod"
    }
  }

  object SomeCommand {
    def unapply(x : String) : Option[String] = {
      if (x.startsWith(":")) Some(x.tail) else None
    }
  }

  private var mode:Mode = ExprMode
  
  private val baseClass = ClassName("Interpreter"::"sl2"::Nil, "Session", Nil)
    
  private var current = Program(Nil, Map(), Map(), Nil)

  def config = Config(Path("."), Nil, Path("."), "", Path("."), 
                      Path.createTempDirectory(deleteOnExit=false))
  
  def banner = "Simulation Language Interpreter"
    
  def currentContext(p : Program) = {    
    for (
      // check and load dependencies
      dependencies <- inferDependencies(p, config).right;
      // type check the program
      _ <- checkProgram(current, normalizeModules(dependencies)).right            
    ) yield context(current, dependencies)      
  }

  var exprCounter : Int = 0
    
  def className : ClassName = {
    baseClass $ ("Expr_" + exprCounter)
  }

  def updateProg(line : String) = {
    for (e <- parseExpr(line).right) yield 
      current.copy(functionDefs = current.functionDefs + ("eval" -> (FunctionDef(Nil, e)::Nil)))
  }

  def processline (line : String) = {
    line match {
      case SwitchModuleMode() => mode = ModMode
      case SwitchExprMode() => mode = ExprMode
      case SomeCommand(x) => emitter.emitln("No comprendo: '" + x + "'")
      case _ => mode match {
        case ExprMode => {
          val res : Either[Error, List[JVMClass]] = compileExpr(className, "eval", line)
          
          res match {
            case Right(classes) => {
              emitter.emitln(eval(className, classes))
              exprCounter = exprCounter + 1
            }
            case Left(err) => emitter.emitln(err)
          }
        }
        case ModMode => {
          emitter.emit("Module mode NYI")
        }
      }
    }
  }
      
  override def prompt = mode match {
    case ExprMode => "SL[e]>"
    case ModMode => "SL[m]>"
  }
}
