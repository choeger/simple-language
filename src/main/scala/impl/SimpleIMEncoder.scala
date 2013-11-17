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

/**
  * Direct encoding of Expr into IMCode
  */
trait SimpleIMEncoder extends IMEncoder with Syntax with SyntaxTraversal with IMSyntax with WadlerPatternMatching with Configs {

  /**
    * Intermediate encoding state, contains name counter and created closures
    */
  sealed case class IMEncodingState(closures : List[IMClosure] = Nil, 
                                    freshName : Int = 0, 
                                    names : Map[String, IMCode] = Map()
                                  )

  /**
    * encoder state monad
    */
  type Encoder[A] = State[IMEncodingState, A]

  /**
    * A simplified (nested) lambda without SL's pattern abstractions
    */
  sealed case class SimpleLambda(arg : IMName, args : List[IMName], code : Expr) 

  /**
    *  Convert an SL-lambda into a SimpleLambda
    */
  def convertLambda(l : Lambda) : SimpleLambda = l match {
    case Lambda(p::Nil, code, _) => p match {
        case PatternVar(x, _) => {
          SimpleLambda(x, Nil, code) 
        }
        case p@_ => {
          SimpleLambda("_arg0", Nil, Case(ExVar(Var("_arg0")), Alternative(p, code)::Nil))
        }
    }
    
    case Lambda(p::ps, rhs, _) => {
      val SimpleLambda(arg, args, code) = convertLambda(Lambda(ps, rhs))

      p match {
        case PatternVar(x, _) => {
          SimpleLambda(x, arg::args, code) 
        }
        case p@_ => {
          SimpleLambda("_arg" + ps.size, arg::args, Case(ExVar(Var("_arg" + ps.size)), Alternative(p, code)::Nil))
        }
      }
    }
  }

  private val g : Graph[String] = new GraphImpl[String]() {}

  private def addClosure(code : IMCode) : Encoder[IMClosure] = { 
    State({s:IMEncodingState => {
      val cls = IMClosure(s.closures.size, code)
      (s.copy(closures=cls::s.closures), cls)}})
  }

  implicit def lift(c : IMCode) : Encoder[IMCode] = State({s:IMEncodingState => (s, c)})

  def freshVarName : Encoder[IMName] = {
    for (s <- get[IMEncodingState]; c=s.freshName; _ <- put(s.copy(freshName=c+1))) yield "#tmp_"+c
  }


  /**
    *  Encode an SL-module
    */
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
          Some(for (r <- enc(env.names, expr)) yield SimplePattern(env.currentModule, c, vars, r))
        case Alternative(PatternExpr(ConVar(c, mod), onlyVars(vars), _), expr, _) =>
          Some(for (r <- enc(env.names, expr)) yield SimplePattern(env.modules(mod), c, vars, r))
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

    def zipWithLength[T](l : List[T]) : List[(T, Int)] = l match {
      case Nil => Nil
      case x::rest => (x, rest.size)::zipWithLength(rest)
    }
    
    def closureEnv(fv: List[String]) = zipWithLength(fv).map { 
      case(k,v) => k -> IMClosureVar(v) 
    }

    def encClosure(arg : String, rest : List[String], names : Map[String, IMCode], 
                   closed: List[String], rhs : Expr) : Encoder[IMCode] = rest match {     
      case Nil => {
        val localEnv = names ++ closureEnv(closed)
        enc(localEnv + (arg -> IMArgument(0)), rhs) ;
      }
      case x::rest => {
        val closed2 = arg::closed 
        val localEnv = names ++ closureEnv(closed)

        for {
          c <- encClosure(x, rest, names, closed2, rhs) ;
          cls <- addClosure(c)          
        } yield IMCapture(cls.nr, IMArgument(0)::closed.map(localEnv))
      }
    }

    def encLambda(names : Map[String, IMCode], s : SimpleLambda) : Encoder[IMCode] = s match {

      case SimpleLambda(arg, rest, rhs) => {
        val moduleBound = (env.names.keys.toSet -- names.keys)
        val closed = (fv(rhs) - arg -- rest -- moduleBound).toList
        for (c <- encClosure(arg, rest, names, closed, rhs) ; 
             cls <- addClosure(c))
          yield IMCapture(cls.nr, closed.map(names))
      }
    }

    /**
      * Encode a dependency-sorted group of LET-definitions
      *  @param names the local environment
      *  @param sorted the dependency-sorting
      *  @param defs the definitions (by name)
      *  @param body the LET-body
      *  @return the encoding monad
      */
    def encodeLetDefs(names : Map[String, IMCode], sorted : List[Set[String]], 
      defs : Map[String, Expr], body : Expr) : Encoder[IMCode] = sorted match {

      /* simple case: no more definitions */
      case Nil => enc(names, body)

      /* simple LET, no recursion */
      case scc::rest if scc.size == 1 => {
        for (
          code <- enc(names, defs(scc.head)) ;
          body <- encodeLetDefs(names + (scc.head -> IMLocalVar(scc.head)), rest, defs, body)
        ) yield IMLet(scc.head, code, body)
      }

      /* a recursive group */
      case scc::rest => {
        val group = scc.toList
        val vars = for (s <- scc ; v <- fv(defs(s)) ; if !scc.contains(v)) yield v

        val closed = group.sorted.reverse ++ vars.toList

        val compiled : List[Encoder[Int]] =
        for (s <- group) yield {
          defs(s) match {
            case l@Lambda(_,_,_) => {
              val SimpleLambda(arg, rest, rhs) = convertLambda(l)
              val enc = encClosure(arg, rest, names, closed, rhs)
              for (c <- enc ; cls <- addClosure(c)) yield cls.nr
            }

            case _ => 
              throw new RuntimeException("Non-lambda in recursive def " + scc.mkString("{",", ", "}"))
          }
        }

        val nextEnv = names ++ scc.map { n => (n -> IMLocalVar(n)) }
        /* actually invoke compilation */
        for (
          clsrs <- compiled.sequence ;
          map = Map() ++ group.zip(clsrs) ;
          imb <- encodeLetDefs(nextEnv, rest, defs, body)
        ) yield IMLetRec(map, vars.toList.map(names), imb)
      }
    }

    def dep(x : String, defs : Set[String], rhs : List[Expr]) : List[(String, String)] = {
      for(e <- rhs ; v <- fv(e); if (defs.contains(v)) ) yield (x -> v)
    }

    def enc(names : Map[String, IMCode], s : Expr) : Encoder[IMCode] = s match {
      case l@Lambda(_, _, _) => {
        encLambda(names, convertLambda(l))
      }

      /*
       * LET's may be mututally recursive, so do a dependency analysis first
       */
      case Let(definitions, body, _) => {
        val defs = Map() ++ definitions.map(d => (d.lhs -> d.rhs))
        val boundVars = defs.keys.toSet
        val dependencies = (for(
          v <- boundVars;
          deps <- dep(v, boundVars, defs(v)::Nil))
        yield deps).toList
        val depGraph = g.directedGraph(boundVars, dependencies)
        val sorted = g.topologicalSort(g.stronglyConnectedComponents(depGraph), depGraph)
        encodeLetDefs(names, sorted, defs, body)
      }

      /*
       * Special case: no distinction
       */
      case Case(e, Alternative(PatternVar(x, _), body, _)::Nil, _) => {
        enc(names, inline(x, e)(body))
      }

      case Case(e, as, _) => {
        for {
          name <- freshVarName ;
          matchee <- enc(names, e) ;
          simplified = createSimpleCaseMatch(env.data, as map alt2Equation, Var(name)::Nil ) ;
          simpleMatch <- case2SimpleMatch(simplified)
        } yield IMLet(name, matchee, simpleMatch)
      }
      
      case App(f, e, _) => for {
        imF <- enc(names,f);
        imA <- enc(names,e)
      } yield IMApp(imF, imA)
      
      case Conditional(c, t, e, _) => for {
        imC <- enc(names,c);
        imT <- enc(names,t);
        imE <- enc(names,e)        
      } yield IMIf(imC, imT, imE)

      case ExCon(ConVar(i, LocalMod), _) => env.names(i)
      case ExCon(ConVar(n, mod), _) => {        
        IMStaticAcc(env.modules(mod), ClassField(n, TObject))
      }

      case ExVar(Var(i, LocalMod), _) if names.contains(i) => names(i)
      case ExVar(Var(i, LocalMod), _) => env.names(i)
      case ExVar(Var(i, mod), _) => {
        IMStaticAcc(env.modules(mod), ClassField(i, TObject))
      }

      case ConstInt(v, _) => IMConstInt(v)
      case ConstChar(c, _) => IMConstChar(c)
      case ConstString(s, _) => IMConstString(s)
      case ConstReal(x, _) => IMConstReal(x)
    }

    def patterns2LambdaChain(e : Lambda, p : List[Pattern]) : Lambda = p match {
      case PatternVar(x, _)::ps => Lambda(PatternVar(x)::Nil, patterns2LambdaChain(e, ps))
      case Nil => e
      case p::ps => Lambda(PatternVar("_arg" + ps.size)::Nil, 
        Case(ExVar(Var("_arg" + ps.size)), Alternative(p, patterns2LambdaChain(e, ps))::Nil))
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
        Lambda(PatternVar(arg1,_)::Nil, 
               Case(ExVar(Var(arg2,LocalMod),_), (la@Alternative(p, e, _))::Nil, _), _), 
        Lambda(PatternVar(arg3, _)::Nil, Case(ExVar(Var(arg4,LocalMod),_), as, _), _)) =>
            Lambda(PatternVar(arg1)::Nil, Case(ExVar(Var(arg1,LocalMod)), la::as))
      case _ => throw new RuntimeException("Internal Error during case-merge")
    }

    def encodeFunctionDef(fd : FunctionDef) : Either[Lambda, Expr] = fd.patterns match {
        case p::ps => {
          val l1 = p match {
            case PatternVar(x, _) => Lambda(p::Nil, fd.expr)
            case _ => Lambda(PatternVar("_arg" + ps.size)::Nil, 
              Case(ExVar(Var("_arg" + ps.size)), Alternative(p, fd.expr)::Nil))
          }
          Left(patterns2LambdaChain(l1, ps))
        }
        case Nil => {
          Right(fd.expr)
        }
    }

    def concatLeft[A,B](l : List[Either[A,B]]) : Either[List[A], B] = l match {
      case Nil => Left(Nil)
      case hd::tl => for(l <- hd.left ; rec <- concatLeft(tl).left) yield l::rec
    }

    def encodeFunction(fds : List[FunctionDef]) : Expr = {
      val rhs : Either[List[Lambda], Expr] = concatLeft(fds map encodeFunctionDef)
      (for (lambdas <- rhs.left) yield lambdas.reduce(mergeCases)) match {
        case Right(e) => e
        case Left(l) => l
      }
    }

    def encProgram(p : Program) : Encoder[IMModuleClass] = {
      val defs = p.functionDefs mapValues encodeFunction
      
      val encFields : List[Encoder[IMConstant]] = defs.toList map { 
        case (k, e) => {
          for (im <- enc(Map(), e)) yield IMConstant(k, im)
        }
      }
  
      for (
        fields <- encFields.sequence
      ) yield IMModuleClass(env.currentModule, fields)      
    }
    
    val (state, klazz) = encProgram(p)(IMEncodingState(Nil))    
    klazz.copy(elements = klazz.elements ++ state.closures)
  }
}
