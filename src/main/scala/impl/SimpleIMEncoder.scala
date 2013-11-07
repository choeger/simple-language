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

trait SimpleIMEncoder extends IMEncoder with Syntax with IMSyntax with WadlerPatternMatching with Configs {

  def instanceField = ClassField("instance", TObject)
  
  sealed case class IMEncodingState(classes : List[IMClass] = Nil, freshName : Int = 0)

  type Encoder[A] = State[IMEncodingState, A]

  private def addClass[T <: IMClass](cls : T) : Encoder[T] = { 
    State({s:IMEncodingState => (s.copy(classes=cls::s.classes), cls)})
  }

  implicit def lift(c : IMCode) : Encoder[IMCode] = State({s:IMEncodingState => (s, c)})

  def freshClassName(base : ClassName) : Encoder[ClassName] = {
    for (s <- get) yield base $ s.classes.size.toString
  }

  def freshVarName : Encoder[IMName] = {
    for (s <- get[IMEncodingState]; c=s.freshName; _ <- put(s.copy(freshName=c+1))) yield "#tmp_"+c
  }

  /**
   * Make all closures explicit applications
   * TODO: check re-implementation using SyntaxTraversal's map()
   */
  def explicitClosures(e : Expr) : Expr = e match {
    case Lambda(_, _, _) => {
      def mkAbs(e : Expr, v : VarName) = Lambda(PatternVar(v)::Nil, e)
      def mkApp(e : Expr, v : VarName) = App(e, ExVar(Var(v, LocalMod)))

      val vars = fv(e).toList.collect { case Var(name, LocalMod) => name }
      (((e /: vars)(mkAbs)) /: vars)(mkApp)
    }

    case Case(e, as, _) => {
      Case(e, as map (a => a.copy(expr = explicitClosures(a.expr))))
    }
      
    case App(f, e, _) => App(explicitClosures(f), explicitClosures(e))
      
    case Let(defs, r, _) => Let(defs map (d => d.copy(rhs = explicitClosures(d.rhs))), explicitClosures(r))

    case Conditional(c, t, e, _) => Conditional(explicitClosures(c), explicitClosures(t), explicitClosures(e))

    case _ => e
  }

  def encode(env : IMEncodingEnv, p : Program) : IMModuleClass = {

    object onlyVars {
      def unapply(p : List[Pattern]) : Option[List[IMName]] = p match {
        case Nil => Some(Nil)
        case PatternVar(n,_)::onlyVars(rest) => Some(n::rest)
        case _ => None
      }
    }

    object simplePattern {
      def unapply(a : Alternative) : Option[Encoder[SimplePattern]] = a match {
        case Alternative(PatternExpr(ConVar(c, LocalMod), onlyVars(vars), _), expr, _) =>
          Some(for (r <- enc(expr)) yield SimplePattern(env.currentModule, c, vars, r))
        case Alternative(PatternExpr(ConVar(c, mod), onlyVars(vars), _), expr, _) =>
          Some(for (r <- enc(expr)) yield SimplePattern(env.modules(mod), c, vars, r))
        case _ => None
      }
    }

    object simplePatterns {
      def unapply(a : List[Alternative]) : Option[List[Encoder[SimplePattern]]] = a match {
        case simplePattern(pat)::simplePatterns(rest) => Some(pat::rest)
        case Nil => Some(Nil)
        case _ => None
      }
    }

    def case2SimpleMatch(e : Expr) = e match {
      case Case(ExVar(Var(n, LocalMod), _), simplePatterns(alts), _) => {
        for (e <- alts.sequence) yield IMSimpleMatch(n, e)
      }
      case _ => throw new RuntimeException("Internal Error. Unexpected Pattern: " + e)
    }

    def newClosure(name : ClassName, imB : IMCode) : Encoder[IMClosureClass] = { for {
      cls <- {
        val cls = IMClosureClass(name, imB)
        addClass(cls)
      }
    } yield (cls) }

    def makeLet(in : IMCode, letDef : LetDef) : Encoder[IMCode] = {
      for {
        d <- enc(letDef.rhs)
      } yield IMLet(letDef.lhs, d, in)
    }

    def makeClosure(name : ClassName, body : Expr, p : Pattern) : Encoder[IMClosureClass] = {
      for(im <- enc(Case(ExVar(Var("arg")), Alternative(p, body)::Nil)) ; cls <- newClosure(name, im)) yield cls
    }

    /* we expect no free vars in the input, closure conversion first! */
    def enc(s : Expr) : Encoder[IMCode] = s match {
      case l@Lambda(_, _, _) => {
        for {
          name <- freshClassName(env.currentModule) ;
          cls <- createNamedClosure(l, name)
        } yield IMStaticAcc(cls.klazz.name, instanceField)
      }

      case Case(e, as, _) => {
        for {
          name <- freshVarName ;
          matchee <- enc(e) ;
          simplified = createSimpleCaseMatch(env.data, as map alt2Equation, Var(name)::Nil ) ;
          simpleMatch <- case2SimpleMatch(simplified)
        } yield IMLet(name, matchee, simpleMatch)
      }
      
      case App(f, e, _) => for {
        imF <- enc(f);
        imA <- enc(e)
      } yield IMApp(imF, imA)
      
      case Let(defs, r, _) =>
        for {
          b <- enc(r) ;
          let <- defs.foldLeftM(b)(makeLet) 
        } yield let

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

    def pattern2Expr(e : Lambda, p : Pattern) : Lambda = {
      Lambda(PatternVar("arg")::Nil, Case(ExVar(Var("arg")), Alternative(p, e)::Nil))
    }

    /**
     * Merge two expressions of the form
     * \\arg . CASE arg OF P1 THEN e1
     * \\arg . CASE arg OF ...
     * into:
     * \\arg . CASE arg OF P1 THEN e1
     *                  OF ...
     */
    def mergeCases(l : Expr, r : Expr) : Lambda = (l,r) match {
      case (
        Lambda(PatternVar("arg",_)::Nil, 
               Case(ExVar(Var("arg",LocalMod),_), (la@Alternative(p, e, _))::Nil, _), _), 
        Lambda(PatternVar("arg", _)::Nil, Case(ExVar(Var("arg",LocalMod),_), as, _), _)) =>
            Lambda(PatternVar("arg")::Nil, Case(ExVar(Var("arg",LocalMod)), la::as))
      case _ => throw new RuntimeException("Internal Error during case-merge")
    }

    def encodeFunctionDef(fd : FunctionDef) : Either[Lambda, Expr] = fd.patterns match {
        case p::ps => {
          val l1 = Lambda(PatternVar("arg")::Nil, Case(ExVar(Var("arg")), Alternative(p, fd.expr)::Nil))
          Left((l1 /: ps)(pattern2Expr))
        }
        case Nil => {
          Right(fd.expr)
        }
    }

    def concatLeft[A,B](l : List[Either[A,B]]) : Either[List[A], B] = l match {
      case Nil => Left(Nil)
      case hd::tl => for(l <- hd.left ; rec <- concatLeft(tl).left) yield l::rec
    }

    def encodeFunction(fds : List[FunctionDef]) : Either[Lambda, Expr] = {
      val rhs : Either[List[Lambda], Expr] = concatLeft(fds map encodeFunctionDef)
      for (lambdas <- rhs.left) yield lambdas.reduce(mergeCases)
    }

    def createNamedClosure(l : Lambda, name : ClassName) : Encoder[IMSubClass] = l match {
      case Lambda(ps, e, _) => ps.reverse match {
        case p :: Nil => {
          for {
            cls <- makeClosure(name, e, p)
          } yield IMSubClass(cls)
        }
        case p :: rest => {
          for {
            cls <- makeClosure(name, Lambda(rest, e), p)
          } yield IMSubClass(cls)
        }
        case Nil => {
          for (im <- enc(e); cls <- newClosure(name, im)) yield IMSubClass(cls)
        }
      }
    }

    def createNamedConstant(e : Expr, name : String) : Encoder[IMModuleElement] = {
      for(im <- enc(e)) yield IMConstant(name, im)
    }

    def encProgram(p : Program) : Encoder[IMModuleClass] = {
      val defs = p.functionDefs mapValues encodeFunction
      
      val encFields = defs.toList map { 
        case (k,Left(lambda)) => createNamedClosure(lambda, env.currentModule $ k)
        case (k,Right(constant)) => createNamedConstant(constant, k)
      }
  
      for (
        fields <- encFields.sequence
      ) yield IMModuleClass(env.currentModule, fields)      
    }
    
    val (state, klazz) = encProgram(p)(IMEncodingState(Nil))    
    klazz.copy(elements = klazz.elements ++ (state.classes map (IMSubClass(_))))
  }
}
