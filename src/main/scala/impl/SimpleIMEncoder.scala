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

import de.tuberlin.uebb.sl2.modules.Syntax._

import scalaz._
import Scalaz._

import scala.language.implicitConversions

trait SimpleIMEncoder extends IMEncoder with Syntax with IMSyntax {

  def instanceField = ClassField("instance", TObject)

  def tagField(klazz:ClassName) = ClassField("tag", TClass(klazz $ "ETag") )
  
  sealed case class IMEncodingState(classes : List[IMClass])

  type Encoder[A] = State[IMEncodingState, A]

  private def addClass(cls : IMClass) : Encoder[IMClass] = { 
    State({s:IMEncodingState => (s.copy(classes=cls::s.classes), cls)})
  }

  implicit def lift(c : IMCode) : Encoder[IMCode] = State({s:IMEncodingState => (s, c)})

  def freshClassName(base : ClassName) : Encoder[ClassName] = {
    for (s <- get) yield base $ s.classes.size.toString
  }

  def encode(env : IMEncodingEnv, s : Expr) : (IMCode, List[IMClass]) = {

    def newClosure(e : Expr, vars : List[String]) : Encoder[IMClass] = { for {
      imB <- enc(e) ;
      name <- freshClassName(env.currentModule) ;
      cls <- {
        val cls = IMClosureClass(name, vars map (ClassField(_, TObject)), imB)
        addClass(cls)
      }
    } yield (cls) }


    def enc(s : Expr) : Encoder[IMCode] = s match {
      case Lambda(ps, e, _) => {
        val vars = (for (Var(name,mod) <- fv(s); 
                         if mod == LocalMod) 
                    yield name) -- env.localNames
        
        for {
          cls <- newClosure(e, vars.toList)
        } yield IMStaticAcc(cls.name, instanceField)
      }

      //case Case(e, as, _) => 
      
      case App(f, e, _) => for {
        imF <- enc(f);
        imA <- enc(e)
      } yield IMApp(imF, imA)
      
      /*
      case Let(defs, r, _) => for { 
        imB <- enc(e);
        imR <- enc(r)
      } yield IMLet(ds, imB, imR ) 
      */

      case Conditional(c, t, e, _) => for {
        imC <- enc(c);
        imT <- enc(t);
        imE <- enc(e)        
      } yield IMIf(imC, imT, imE)

      case ExCon(ConVar(i, LocalMod), _) if env.localNames contains i => 
        IMStaticAcc(env.currentModule $ i, instanceField)
      case ExCon(ConVar(n, LocalMod), _) => IMVar(n)
      case ExCon(ConVar(n, mod), _) => IMStaticAcc(env.modules(mod), instanceField)

      case ExVar(Var(i, LocalMod), _) if env.localNames contains i => 
        IMStaticAcc(env.currentModule $ i, instanceField)
      case ExVar(Var(i, LocalMod), _) => IMVar(i)
      case ExVar(Var(i, mod), _) => IMStaticAcc(env.modules(mod), instanceField)

      case ConstInt(v, _) => IMConstInt(v)
      case ConstChar(c, _) => IMConstChar(c)
      case ConstString(s, _) => IMConstString(s)
      case ConstReal(x, _) => IMConstReal(x)
    }
    
    val (state, code) = enc(s)(IMEncodingState(Nil))    
    (code, state.classes)
  }
}
